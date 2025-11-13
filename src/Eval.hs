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

-- | Big-step evaluator for expressions, just for Var and Mu | --
evalExpr :: Env -> Store -> Expr -> (Value, Store)
evalExpr env store (Var x) = (storeLookup store (envLookup env x), store)
evalExpr env store (Mu clauses) = (MuValue env clauses, store)
evalExpr _ store (Cons ident []) = (ConsValue ident [], store)
evalExpr _ _ (Cons _ (_:_)) = 
  error "evalExpr: Non-empty constructors must be evaluated in small-step"

-- | Helper function to collapse completed frames when multiple finish at once | --
collapseFrames :: [ConsFrame] -> Value -> Env -> Store -> Expr -> [Config]
collapseFrames [] value env store ce =
  -- All frames collapsed, now handle the context (the coexpr)
  case ce of
    -- eval one by one for a arged constructor
    Cons cId (arg:args) -> 
      [CoConsEvalConfig env store value [ConsFrame cId [] args] arg]
    -- direct eval for empty constructor, mu, or var
    _ ->
      let (coValue, store') = evalExpr env store ce
      in [ValueConfig store' value coValue]
collapseFrames (ConsFrame parentId parentEvaled [] : rest) value env store ce =
  -- Parent frame is also done, keep collapsing
  let parentValue = ConsValue parentId (parentEvaled ++ [value]) -- possible optimization: avoid reconstructing lists
  in collapseFrames rest parentValue env store ce
collapseFrames (ConsFrame parentId parentEvaled (nextArg:remaining) : rest) value env store ce =
  -- Parent frame has more args, continue evaluation
  [ConsEvalConfig env store (ConsFrame parentId (parentEvaled ++ [value]) remaining : rest) nextArg ce]

-- | Helper function to collapse completed frames for covalue side | --
collapseCoFrames :: [ConsFrame] -> Value -> Value -> Env -> Store -> [Config]
collapseCoFrames [] value coValue _env store =
  -- All frames collapsed, create ValueConfig
  [ValueConfig store value coValue]
collapseCoFrames (ConsFrame parentId parentEvaled [] : rest) value coValue env store =
  -- Parent frame is also done, keep collapsing
  let parentCoValue = ConsValue parentId (parentEvaled ++ [coValue])
  in collapseCoFrames rest value parentCoValue env store
collapseCoFrames (ConsFrame parentId parentEvaled (nextArg:remaining) : rest) value coValue env store =
  -- Parent frame has more args, continue evaluation
  [CoConsEvalConfig env store value (ConsFrame parentId (parentEvaled ++ [coValue]) remaining : rest) nextArg]

-- || Small-step evaluator for the CESK machine || --
step :: Config -> [Config]

-- COMMAND EVALUATION --

-- Command Variable:

-- look up in store
step (CommandConfig env store (CommandVar cmdId)) =
  [ CommandConfig
      env
      store
      (storeLookupCommand store (envLookup env cmdId))
  ]

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

-- +++ Cases for non-empty frame stack +++ --

-- 1. Evaluating a nested non-empty constructor - push new frame
step (ConsEvalConfig env store frames (Cons consId (arg:args)) ce) =
  [ConsEvalConfig env store (ConsFrame consId [] args : frames) arg ce]
-- 2. Evaluating an empty nested constructor
step (ConsEvalConfig env store (frame:restFrames) (Cons consId []) ce) =
  -- Empty nested constructor - add to parent frame
  let emptyConsValue = ConsValue consId []
      ConsFrame parentId parentEvaled remainingArgs = frame
  in case remainingArgs of
       (nextArg:rest) ->
         [ConsEvalConfig env store (ConsFrame parentId (parentEvaled ++ [emptyConsValue]) rest : restFrames) nextArg ce]
       [] ->
         -- Parent frame is also done, collapse through all completed frames
         let parentConsValue = ConsValue parentId (parentEvaled ++ [emptyConsValue])
         in collapseFrames restFrames parentConsValue env store ce

-- +++ Cases for empty frame stack ++ --
step (ConsEvalConfig env store [] (Cons consId []) ce) =
  -- Empty constructor at top level - no frames
  let (coValue, store') = evalExpr env store ce
  in [ValueConfig store' (ConsValue consId []) coValue]

-- Base case: empty frames with simple values means we're done (shouldn't happen in normal flow)
step (ConsEvalConfig env store [] (Var x) ce) =
  let (coValue, store') = evalExpr env store ce
      value = storeLookup store (envLookup env x)
  in [ValueConfig store' value coValue]
step (ConsEvalConfig env store [] (Mu clauses) ce) =
  let (coValue, store') = evalExpr env store ce
  in [ValueConfig store' (MuValue env clauses) coValue]
step (ConsEvalConfig env store (ConsFrame consId evaledArgs (nextArg:remainingArgs) : restFrames) (Var x) ce) =
  -- Evaluated a simple value, add it to current frame and continue with next arg
  [ConsEvalConfig env store' (ConsFrame consId (evaledArgs ++ [value]) remainingArgs : restFrames) nextArg ce]
  where
    value = storeLookup store (envLookup env x)
    store' = store
step (ConsEvalConfig env store (ConsFrame consId evaledArgs (nextArg:remainingArgs) : restFrames) (Mu clauses) ce) =
  -- Evaluated a Mu value, add it to current frame and continue with next arg
  [ConsEvalConfig env store (ConsFrame consId (evaledArgs ++ [MuValue env clauses]) remainingArgs : restFrames) nextArg ce]
step (ConsEvalConfig env store (ConsFrame consId evaledArgs [] : restFrames) (Var x) ce) =
  -- Last argument of this constructor, pop frame
  let value = storeLookup store (envLookup env x)
      consValue = ConsValue consId (evaledArgs ++ [value])
  in collapseFrames restFrames consValue env store ce
step (ConsEvalConfig env store (ConsFrame consId evaledArgs [] : restFrames) (Mu clauses) ce) =
  -- Last argument is Mu, pop frame
  let consValue = ConsValue consId (evaledArgs ++ [MuValue env clauses])
  in collapseFrames restFrames consValue env store ce

-- Duo of the above for coexpression side --
step (CoConsEvalConfig env store value frames (Cons consId (arg:args))) =
  [CoConsEvalConfig env store value (ConsFrame consId [] args : frames) arg]
step (CoConsEvalConfig env store value (frame:restFrames) (Cons consId [])) =
  -- Empty nested constructor in covalue
  let emptyConsValue = ConsValue consId []
      ConsFrame parentId parentEvaled remainingArgs = frame
  in case remainingArgs of
       (nextArg:rest) ->
         [CoConsEvalConfig env store value (ConsFrame parentId (parentEvaled ++ [emptyConsValue]) rest : restFrames) nextArg]
       [] ->
         -- Parent frame is also done, collapse through all completed frames
         let parentConsValue = ConsValue parentId (parentEvaled ++ [emptyConsValue])
         in collapseCoFrames restFrames value parentConsValue env store
step (CoConsEvalConfig _ store value [] (Cons consId [])) =
  [ValueConfig store value (ConsValue consId [])]
step (CoConsEvalConfig env store value [] (Var x)) =
  let coValue = storeLookup store (envLookup env x)
  in [ValueConfig store value coValue]
step (CoConsEvalConfig env store value [] (Mu clauses)) =
  [ValueConfig store value (MuValue env clauses)]
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs (nextArg:remainingArgs) : restFrames) (Var x)) =
  let argValue = storeLookup store (envLookup env x)
  in [CoConsEvalConfig env store value (ConsFrame consId (evaledArgs ++ [argValue]) remainingArgs : restFrames) nextArg]
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs (nextArg:remainingArgs) : restFrames) (Mu clauses)) =
  [CoConsEvalConfig env store value (ConsFrame consId (evaledArgs ++ [MuValue env clauses]) remainingArgs : restFrames) nextArg]
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs [] : restFrames) (Var x)) =
  let argValue = storeLookup store (envLookup env x)
      coConsValue = ConsValue consId (evaledArgs ++ [argValue])
  in collapseCoFrames restFrames value coConsValue env store
step (CoConsEvalConfig env store value (ConsFrame consId evaledArgs [] : restFrames) (Mu clauses)) =
  let coConsValue = ConsValue consId (evaledArgs ++ [MuValue env clauses])
  in collapseCoFrames restFrames value coConsValue env store

-- VALUE CONFIGURATIONS --

-- Constructor and MuValue matching
step (ValueConfig store cons@(ConsValue {}) (MuValue env clauses)) =
  match env store cons clauses
-- ComuValue and Coconstructor matching
step (ValueConfig store (MuValue env clauses) cons@(ConsValue {})) =
  match env store cons clauses

-- Both single-clause Var pattern MuValues matching
step (ValueConfig store v@(MuValue env muClaus@[(VarPattern _, _)]) cv@(MuValue env' comuClaus@[(VarPattern _, _)])) =
  match env store cv muClaus -- ++ match env store cv muClaus THIS IS A TEMPORARY HACK

-- Both MuValues with multiple clauses matching
step (ValueConfig store v@(MuValue env clauses) cv@(MuValue env' clauses')) =
  match env' store v clauses' ++ match env store cv clauses

-- temporary hack to deal with "Halt"
step (ValueConfig _ cons@(ConsValue _ _) (ConsValue "Halt" [])) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]
step (ValueConfig _ (ConsValue "Halt" []) cons@(ConsValue _ _)) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]

-- Error: both sides are constructors
step (ValueConfig _ (ConsValue {}) (ConsValue {})) =
  [ErrorConfig "Bad type: cannot continue with 2 constructors"]

-- ERROR CONFIGURATION --
step (ErrorConfig {}) = []

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
initStore = Store (Addr 0) (Addr 0) Map.empty Map.empty

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
