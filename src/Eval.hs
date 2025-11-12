module Eval
  ( 
    step,
    evalProgram,
    foldM,
    evalDecls,
    initEnv,
    initStore,
    Config (..),
    tryMatch,
  )
where

import qualified Data.Map as Map
import Pretty (prettyTopLevelValue, renderPretty)
import Syntax
    ( Env(..),
      Store(..),
      Expr(..),
      Value(..),
      VarId,
      Addr(Addr),
      Pattern(..),
      Command(..),
      ConsFrame(..),
      Config(..),
      OneHoleContext(..),
      Decl(..),
      Program(Program),
      storeLookup,
      envLookup,
      storeLookupCommand,
      freshSubstVar,
      envStoreInsert )
import GHC.RTS.Flags (GCFlags(oldGenFactor))

-- | Big-step evaluator for expressions, just for Var and Mu | --
evalExpr :: Env -> Store -> Expr -> (Value, Store)
evalExpr env store (Var x) = (storeLookup store (envLookup env x), store)
evalExpr env store (Mu clauses) = (MuValue env clauses, store)
evalExpr _ store (Cons ident []) = (ConsValue ident [], store)
evalExpr _ _ (Cons _ (_:_)) = 
  error "evalExpr: Non-empty constructors must be evaluated in small-step"
evalExpr _ _ (DelimExpr _) =
  error "evalExpr: Delimited expressions must be evaluated in small-step"

-- | Helper function to collapse completed frames when a frame finishes | --
popFrame :: [ConsFrame] -> Value -> Env -> Store -> Expr -> [Config]
popFrame [] value env store ce =
  -- All frames collapsed, now handle the context (the coexpr)
  case ce of
    -- eval one by one for a arged constructor
    Cons cId (arg:args) -> 
      [CoConsEvalConfig env store value [ConsFrame cId [] args] arg]
    -- direct eval for empty constructor, mu, or var
    _ ->
      let (coValue, store') = evalExpr env store ce
      in [ValueConfig store' value coValue]
popFrame (ConsFrame parentId parentEvaled [] : rest) value env store ce =
  -- Parent frame is also done, keep collapsing
  let parentValue = ConsValue parentId (parentEvaled ++ [value]) -- TODO: possible optimization: avoid reconstructing lists
  in popFrame rest parentValue env store ce
popFrame (ConsFrame parentId parentEvaled (nextArg:remaining) : rest) value env store ce =
  -- Parent frame has more args, continue evaluation
  [ConsEvalConfig env store (ConsFrame parentId (parentEvaled ++ [value]) remaining : rest) nextArg ce]

-- | Helper function to collapse completed frames for covalue side | --
popCoFrame :: [ConsFrame] -> Value -> Value -> Env -> Store -> [Config]
popCoFrame [] value coValue _env store =
  -- All frames collapsed, create ValueConfig
  [ValueConfig store value coValue]
popCoFrame (ConsFrame parentId parentEvaled [] : rest) value coValue env store =
  -- Parent frame is also done, keep collapsing
  let parentCoValue = ConsValue parentId (parentEvaled ++ [coValue])
  in popCoFrame rest value parentCoValue env store
popCoFrame (ConsFrame parentId parentEvaled (nextArg:remaining) : rest) value coValue env store =
  -- Parent frame has more args, continue evaluation
  [CoConsEvalConfig env store value (ConsFrame parentId (parentEvaled ++ [coValue]) remaining : rest) nextArg]

-- || Small-step evaluator for the CESK machine || --
-- value/covalue judgements for Mini-Mu.
-- Of note, any single-armed variable binder Mu is NOT a value.
-- eg: { x -> CMD } NOT VALUE
-- eg: { ConsPattern ... -> CMD } IS VALUE
-- eg: { branch1 -> CMD1 | branch2 -> CMD2 } IS VALUE
isValue :: Value -> Bool
isValue (MuValue _ []) = True -- absurd
isValue (MuValue _ [(ConsPattern _ _, _)]) = True
isValue (MuValue _ (_ : _ : _)) = True
isValue (MuValue _ [(VarPattern _, _)]) = False
isValue (MuValue _ [(WildcardPattern, _)]) = False
isValue (ConsValue _ elems) = all isValue elems

-- work in progress. we might need to introduce fresh variables here.
focus :: Store -> Either (ConsId, [Value]) (ConsId, [Value]) -> Env -> [(Pattern, Command)] -> [Config]
focus store value muEnv muClauses =
  let (consId, vals) =
        case value of
          Left (cid, vls) -> (cid, vls)
          Right (cid, vls) -> (cid, vls)
  in 
  case span isValue vals of
    (_, []) -> match muEnv store (ConsValue consId vals) muClauses
    (before, v : _) ->
      let oldMu = Mu muClauses
          v_index = length before
      -- create a new variable for each before and after value,
      -- named _N after their index in the list
          focus_vars = map (("_focus" ++) . show) [0 .. length vals - 1]
          newCons = Cons consId (map Var focus_vars)
      -- insert all of the focsus variables into the environment
          (newEnv, newStore) = foldl
            (\(e, s) (var, val) -> envStoreInsert e s var val)
            (muEnv, store)
            (zip focus_vars vals)
      in 
      -- finally, cut v against a freshly constructed mu-expression
      case value of
        Left _ ->
          [ ValueConfig
              newStore
              v
              (MuValue newEnv [(VarPattern (focus_vars !! v_index), Command newCons oldMu)])
          ]
        Right _ ->
          [ ValueConfig
              newStore
              (MuValue newEnv [(VarPattern (focus_vars !! v_index), Command newCons oldMu)])
              v
          ]

step :: Config -> [Config]

-- +++ BEGIN OF STEP FUNCTION +++ --

-- COMMAND EVALUATION --

-- Command Variable:

-- look up in store
step (CommandConfig env store (CommandVar cmdId)) =
  [ CommandConfig
      env
      store
      (storeLookupCommand store (envLookup env cmdId))
  ]

-- Delimited Expression cases:

-- DelimExpr on expression side
step (CommandConfig env store (Command (DelimExpr innerCmd) ce)) =
  let (substVar, store') = freshSubstVar store
      parentConfig = CommandConfig env store' (Command (Var substVar) ce)
  in [DelimConfig (CommandConfig env store' innerCmd) (Context substVar parentConfig)]

-- DelimExpr on coexpression side
step (CommandConfig env store (Command e (DelimExpr innerCmd))) =
  let (substVar, store') = freshSubstVar store
      parentConfig = CommandConfig env store' (Command e (Var substVar))
  in [DelimConfig (CommandConfig env store' innerCmd) (Context substVar parentConfig)]

-- Concrete Command cases:

{- Notice that there are 2 general cases that need to goes into (co)ConsEvalConfig
   1. Non-empty constructor, anything
   2. anything, Non-empty constructor -}

-- 1. Non-empty constructor paired with anything
step (CommandConfig env store (Command (Cons consId (arg:args)) ce)) =
  [ConsEvalConfig env store [ConsFrame consId [] args] arg ce]

-- 2. Or Anything paired with non-empty constructor
step (CommandConfig env store (Command e (Cons cId (arg : args)))) =
  let (value, store') = evalExpr env store e
   in [CoConsEvalConfig env store' value [ConsFrame cId [] args] arg]

{- Notice that there are 4 cases that do not need to goes into (co)ConsEvalConfig
   1. empty constructor with empty constructor
   2. empty constructor with non-constructor coexpr (Var or Mu)
   3. non-constructor expr (Var or Mu) with empty constructor
   4. Both sides are non-constructors (Var or Mu) -}

-- 1. Both sides are empty constructors
step (CommandConfig _ store (Command (Cons consId []) (Cons consId' []))) =
  [ValueConfig store (ConsValue consId []) (ConsValue consId' [])]

-- 2. Empty constructor with non-constructor coexpr (Var or Mu)
-- notice ce can be only Var or Mu here, other cases are handled above
step (CommandConfig env store (Command (Cons consId []) ce)) =
  [ValueConfig store' (ConsValue consId []) coValue]
  where
    (coValue, store') = evalExpr env store ce

-- 3. Non-constructor expr (Var or Mu) with empty constructor coexpr
step (CommandConfig env store (Command e (Cons cId []))) =
  let (value, store') = evalExpr env store e
  in [ValueConfig store' value (ConsValue cId [])]

-- 4. Both sides are non-constructors (Var or Mu)
step (CommandConfig env store (Command e ce)) =
  [ValueConfig store'' value coValue]
  where
    (value, store') = evalExpr env store e
    (coValue, store'') = evalExpr env store' ce

-- CONSTRUCTOR EVALUATION WITH FRAMES --

-- +++ Cases of encounter non-empty constructor: Always push +++ --

-- 1. Evaluating a nested non-empty constructor - push new frame
step (ConsEvalConfig env store frames (Cons consId (arg:args)) ce) =
  [ConsEvalConfig env store (ConsFrame consId [] args : frames) arg ce]

-- 2. Evaluating DelimExpr - create delimited context
step (ConsEvalConfig env store frames (DelimExpr innerCmd) ce) =
  let (substVar, store') = freshSubstVar store
      parentConfig = ConsEvalConfig env store' frames (Var substVar) ce
  in [DelimConfig (CommandConfig env store' innerCmd) (Context substVar parentConfig)]


-- +++ Cases for non-empty frame stack, with simple exprs +++ --

-- 1. Non-empty frame with simple values (Cons)
step (ConsEvalConfig env store (ConsFrame pconsId evaledArgs (nextArg:remainingArgs) : restFrames) (Cons consId []) ce) =
  -- Evaluated an empty constructor, add it to current frame and continue with next arg
  let emptyConsValue = ConsValue consId [] in
  [ 
    ConsEvalConfig 
    env 
    store 
    (ConsFrame pconsId (evaledArgs ++ [emptyConsValue]) remainingArgs : restFrames) 
    nextArg 
    ce
  ]

-- 2. Non-empty frame with simple values (Var)
step (ConsEvalConfig env store (ConsFrame consId evaledArgs (nextArg:remainingArgs) : restFrames) (Var x) ce) =
  -- Evaluated a simple value, add it to current frame and continue with next arg
  [
    ConsEvalConfig 
    env 
    store' 
    (ConsFrame consId (evaledArgs ++ [value]) remainingArgs : restFrames) 
    nextArg 
    ce
  ]
  where
    value = storeLookup store (envLookup env x)
    store' = store

-- 3. Non-empty frame with simple values (Mu)
step (ConsEvalConfig env store (ConsFrame consId evaledArgs (nextArg:remainingArgs) : restFrames) (Mu clauses) ce) =
  -- Evaluated a Mu value, add it to current frame and continue with next arg
  [
    ConsEvalConfig 
    env 
    store 
    (ConsFrame consId (evaledArgs ++ [MuValue env clauses]) remainingArgs : restFrames) 
    nextArg 
    ce
  ]

{- * Special cases for completing frames, with simple exprs *
     In which cases, we will pop the frame -}

-- 4. Last argument of this constructor, pop frame (Empty Cons)
step (ConsEvalConfig env store (ConsFrame pconsId evaledArgs [] : restFrames) (Cons consId []) ce) =
  let parentConsValue = ConsValue pconsId (evaledArgs ++ [emptyConsValue])
      emptyConsValue = ConsValue consId [] 
  in popFrame restFrames parentConsValue env store ce

-- 5. Last argument of this constructor, pop frame (Var)
step (ConsEvalConfig env store (ConsFrame consId evaledArgs [] : restFrames) (Var x) ce) =
  -- Last argument of this constructor, pop frame
  let value = storeLookup store (envLookup env x)
      consValue = ConsValue consId (evaledArgs ++ [value])
  in popFrame restFrames consValue env store ce

-- 6. Last argument of this constructor, pop frame (Mu)
step (ConsEvalConfig env store (ConsFrame consId evaledArgs [] : restFrames) (Mu clauses) ce) =
  -- Last argument is Mu, pop frame
  let consValue = ConsValue consId (evaledArgs ++ [MuValue env clauses])
  in popFrame restFrames consValue env store ce

-- +++ Cases for empty frame stack ++ --

-- 1. Empty frames with empty constructor
step (ConsEvalConfig env store [] (Cons consId []) ce) =
  -- Empty constructor at top level - no frames
  let (coValue, store') = evalExpr env store ce
  in [ValueConfig store' (ConsValue consId []) coValue]

-- 2. Empty frames with simple values (Var)
step (ConsEvalConfig env store [] (Var x) ce) =
  let (coValue, store') = evalExpr env store ce
      value = storeLookup store (envLookup env x)
  in [ValueConfig store' value coValue]

-- 2. Empty frames with simple values (Mu)
step (ConsEvalConfig env store [] (Mu clauses) ce) =
  let (coValue, store') = evalExpr env store ce
  in [ValueConfig store' (MuValue env clauses) coValue]

-- +++ Dual of the above, for coexpression side +++ --

-- +++ Cases of encounter non-empty co-constructor: Always push +++ --

-- 1. Evaluating a nested non-empty co-constructor - always push a new frame
step (CoConsEvalConfig env store value frames (Cons consId (arg:args))) =
  [CoConsEvalConfig env store value (ConsFrame consId [] args : frames) arg]

-- 2. Evaluating DelimExpr - create delimited context
step (CoConsEvalConfig env store value frames (DelimExpr innerCmd)) =
  let (substVar, store') = freshSubstVar store
      parentConfig = CoConsEvalConfig env store' value frames (Var substVar)
  in [DelimConfig (CommandConfig env store' innerCmd) (Context substVar parentConfig)]

-- +++ Cases for non-empty frame stack, with simple coexprs +++ --

-- 1. Non-empty frame with simple covalues (Cons)
step (CoConsEvalConfig env store value (ConsFrame pconsId evaledArgs (nextArg : remainingArgs) : restFrames) (Cons consId [])) =
  -- Empty nested constructor in covalue
  let emptyConsValue = ConsValue consId []
  in [CoConsEvalConfig env store value (ConsFrame pconsId (evaledArgs ++ [emptyConsValue]) remainingArgs : restFrames) nextArg]

-- 2. Non-empty frame with simple covalues (Var)
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs (nextArg : remainingArgs) : restFrames) (Var x)) =
  let argValue = storeLookup store (envLookup env x)
  in [CoConsEvalConfig env store value (ConsFrame consId (evaledArgs ++ [argValue]) remainingArgs : restFrames) nextArg]

-- 3. Non-empty frame with simple covalues (Mu)
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs (nextArg : remainingArgs) : restFrames) (Mu clauses)) =
  [CoConsEvalConfig env store value (ConsFrame consId (evaledArgs ++ [MuValue env clauses]) remainingArgs : restFrames) nextArg]

-- +++ Cases for completing frames with simple coexprs +++ --

-- 4. Last argument of this co-constructor, pop frame (Empty Cons)
step (CoConsEvalConfig env store value (ConsFrame pconsId evaledArgs [] : restFrames) (Cons consId [])) =
  let emptyConsValue = ConsValue consId []
      parentConsValue = ConsValue pconsId (evaledArgs ++ [emptyConsValue])
  in popCoFrame restFrames value parentConsValue env store

-- 5. Last argument of this co-constructor, pop frame (Var)
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs [] : restFrames) (Var x)) =
  let argValue = storeLookup store (envLookup env x)
      coConsValue = ConsValue consId (evaledArgs ++ [argValue])
  in popCoFrame restFrames value coConsValue env store

-- 6. Last argument of this co-constructor, pop frame (Mu)
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs [] : restFrames) (Mu clauses)) =
  let coConsValue = ConsValue consId (evaledArgs ++ [MuValue env clauses])
  in popCoFrame restFrames value coConsValue env store

-- +++ Cases for non-empty frame stack, with simple coexprs +++ --

-- 1. Empty frames with empty constructor
step (CoConsEvalConfig _ store value [] (Cons consId [])) =
  [ValueConfig store value (ConsValue consId [])]

-- 2. Empty frames with simple covalues (Var)
step (CoConsEvalConfig env store value [] (Var x)) =
  let coValue = storeLookup store (envLookup env x)
  in [ValueConfig store value coValue]

-- 3. Empty frames with simple covalues (Mu)
step (CoConsEvalConfig env store value [] (Mu clauses)) =
  [ValueConfig store value (MuValue env clauses)]

-- VALUE CONFIGURATIONS --

-- for these two cases, we depend on matchers being
-- transformed to handle one level of pattern matching at a time.

-- Constructor and MuValue matching
step (ValueConfig store cons@(ConsValue {}) (MuValue env clauses)) =
  match env store cons clauses

-- ComuValue and Coconstructor matching
step (ValueConfig store (MuValue env clauses) cons@(ConsValue {})) =
  match env store cons clauses

-- for matching mu vs mu, we have to consider 4 cases:
-- 1. both sides are values -> error
-- 2. left side is value -> match right side against left clauses
-- 3. right side is value -> match left side against right clauses
-- 4. neither side is value -> match both sides separately and combine results
-- case 4 might be eliminated with a reasonable evaluation strategy in the future.
step (ValueConfig store v@(MuValue env clauses) cv@(MuValue env' clauses')) 
  | isValue v && isValue cv =
    [ErrorConfig "Bad type: A covalue is being matched against a value"]
  | isValue v =
    match env' store v clauses'
  | isValue cv =
    match env store cv clauses
  | otherwise = 
    match env' store v clauses' ++ match env store cv clauses

-- temporary hack to deal with "Halt"
step (ValueConfig _ cons@(ConsValue _ _) (ConsValue "Halt" [])) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]
step (ValueConfig _ (ConsValue "Halt" []) cons@(ConsValue _ _)) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]

-- Error: both sides are constructors
step (ValueConfig _ (ConsValue {}) (ConsValue {})) =
  [ErrorConfig "Bad type: cannot continue with 2 constructors"]

-- DELIMITED CONTINUATION CONFIGURATIONS --

-- Return on expression side - pop delimited context
step (DelimConfig (ValueConfig store value (ConsValue "Return" [])) (Context substVar parentConfig)) =
  let (env', store') = case parentConfig of
        CommandConfig env _ _ -> envStoreInsert env store substVar value
        ConsEvalConfig env _ _ _ _ -> envStoreInsert env store substVar value
        CoConsEvalConfig env _ _ _ _ -> envStoreInsert env store substVar value
        _ -> error $ "Unexpected parent config in DelimConfig" ++ show parentConfig
      updatedParentConfig = case parentConfig of
        CommandConfig _ _ cmd -> CommandConfig env' store' cmd
        ConsEvalConfig _ _ frames expr ce -> ConsEvalConfig env' store' frames expr ce
        CoConsEvalConfig _ _ val frames expr -> CoConsEvalConfig env' store' val frames expr
        _ -> error $ "Unexpected parent config in DelimConfig" ++ show parentConfig
  in [updatedParentConfig]

-- Return on coexpression side - pop delimited context
step (DelimConfig (ValueConfig store (ConsValue "Return" []) value) (Context substVar parentConfig)) =
  let (env', store') = case parentConfig of
        CommandConfig env _ _ -> envStoreInsert env store substVar value
        ConsEvalConfig env _ _ _ _ -> envStoreInsert env store substVar value
        CoConsEvalConfig env _ _ _ _ -> envStoreInsert env store substVar value
        _ -> error $ "Unexpected parent config in DelimConfig" ++ show parentConfig
      updatedParentConfig = case parentConfig of
        CommandConfig _ _ cmd -> CommandConfig env' store' cmd
        ConsEvalConfig _ _ frames expr ce -> ConsEvalConfig env' store' frames expr ce
        CoConsEvalConfig _ _ val frames expr -> CoConsEvalConfig env' store' val frames expr
        _ -> error $ "Unexpected parent config in DelimConfig" ++ show parentConfig
  in [updatedParentConfig]

-- General case: step the inner config and keep wrap it in DelimConfig
step (DelimConfig innerConfig ctx) =
  map (`DelimConfig` ctx) (step innerConfig)

-- ERROR CONFIGURATION --
step (ErrorConfig {}) = []

-- +++ END OF STEP FUNCTION +++ --

-- | Pattern matching function | --
match :: Env -> Store -> Value -> [(Pattern, Command)] -> [Config]
match _ _ _ [] = []
match env store v ((pat, cmd) : clauses) =
  case tryMatch env store v pat of
    Just (env', store') -> [CommandConfig env' store' cmd]
    Nothing -> match env store v clauses

-- | Core pattern matching logic | --
tryMatch :: Env -> Store -> Value -> Pattern -> Maybe (Env, Store)
tryMatch env store _ WildcardPattern =
  Just (env, store) -- Wildcard always matches
tryMatch env store v (VarPattern x) =
  Just (envStoreInsert env store x v) -- Bind variable
tryMatch env store (ConsValue con args) (ConsPattern con' pats)
  | con == con' && length args == length pats =
      foldM matchArg (env, store) (zip args pats)
  | otherwise = Nothing
  where
    matchArg (env', store') (arg, pat) =
      tryMatch env' store' arg pat
tryMatch _ _ (MuValue {}) (ConsPattern {}) =
  Nothing -- Cannot match MuValue with constructor pattern

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = return acc
foldM f acc (x : xs) = do
  acc' <- f acc x
  foldM f acc' xs


-- let-and-set
-- | Evaluate a list of declarations, returning the updated environment and store | --
evalDecls :: Env -> Store -> [Decl] -> (Env, Store)
evalDecls env store ds = (env', store')
  where
    (env', store') = foldl go (env, store) ds
    go :: (Env, Store) -> Decl -> (Env, Store)
    go (envAccum, storeAccum) (Decl d e) =
      envStoreInsert envAccum storeAccum' d value
      where
        (value, storeAccum') = evalExpr env' storeAccum e

-- CESK machine utils

-- | Initial empty environment | --
initEnv :: Env
initEnv = Env Map.empty Map.empty

-- | Initial empty store | --
initStore :: Store
initStore = Store (Addr 0) (Addr 0) 0 Map.empty Map.empty

-- | A special Halt constructor to signal termination | --
halt :: Expr
halt = Cons "Halt" []

{- | Entrance point for the program evaluation, IGNORE IMPORTS EXPORTS
     Use evalProgramWithDepDecls if you want to handle module dependencies | -}
evalProgram :: Program -> VarId -> Config
evalProgram (Program _ decls _) varId =
  uncurry
    CommandConfig
    (evalDecls initEnv initStore decls) -- Still just eval local decls for now
    (Command (Var varId) halt)
