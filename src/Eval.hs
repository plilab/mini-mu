module Eval
  ( eval,
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
import GHC.RTS.Flags (GCFlags(oldGenFactor))

evalExpr :: Env -> Store -> Expr -> (Value, Store)
evalExpr env store (Var x) = (storeLookup store (envLookup env x), store)
evalExpr env store (Mu clauses) = (MuValue env clauses, store)
evalExpr env store (Cons ident args) = (ConsValue ident argValues, store')
  where
    (argValues, store') = foldl go ([], store) args
    go :: ([Value], Store) -> Expr -> ([Value], Store)
    go (argValuesAccum, storeAccum) arg =
      (argValuesAccum ++ [argValue], storeAccum')
      where
        (argValue, storeAccum') = eval env storeAccum arg

eval :: Env -> Store -> Expr -> (Value, Store)
eval env store expr = (value, store')
  where
    (value, store') = evalExpr env store expr

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
step (CommandConfig env store (Command e ce)) =
  [ValueConfig store'' value coValue]
  where
    (value, store') = evalExpr env store e
    (coValue, store'') = evalExpr env store' ce
step (CommandConfig env store (CommandVar cmdId)) =
  [ CommandConfig
      env
      store
      (storeLookupCommand store (envLookup env cmdId))
  ]
step (ValueConfig store cons@(ConsValue cid vals) (MuValue env clauses)) =
  focus store (Left (cid, vals)) env clauses
step (ValueConfig store (MuValue env clauses) cons@(ConsValue cid vals)) =
  focus store (Right (cid, vals)) env clauses
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
step (ValueConfig _ (ConsValue {}) (ConsValue {})) =
  [ErrorConfig "Bad type: cannot continue with 2 constructors"]
step (ErrorConfig {}) = []

match :: Env -> Store -> Value -> [(Pattern, Command)] -> [Config]
match _ _ _ [] = []
match env store v ((pat, cmd) : clauses) =
  case tryMatch env store v pat of
    Just (env', store') -> [CommandConfig env' store' cmd]
    Nothing -> match env store v clauses

-- Core pattern matching logic
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

-- evalDecls :: [Decl] -> Env
-- let-and-set
evalDecls :: Env -> Store -> [Decl] -> (Env, Store)
evalDecls env store ds = (env', store')
  where
    (env', store') = foldl go (env, store) ds
    go :: (Env, Store) -> Decl -> (Env, Store)
    go (envAccum, storeAccum) (Decl d e) =
      envStoreInsert envAccum storeAccum' d value
      where
        (value, storeAccum') = evalExpr env' storeAccum e

-- cesk machine

initEnv :: Env
initEnv = Env Map.empty Map.empty

initStore :: Store
initStore = Store (Addr 0) (Addr 0) Map.empty Map.empty

halt :: Expr
halt = Cons "Halt" []

-- Entrance point for the program evaluation, IGNORE IMPORTS EXPORTS
evalProgram :: Program -> VarId -> Config
evalProgram (Program _ decls _) varId =
  uncurry
    CommandConfig
    (evalDecls initEnv initStore decls) -- Still just eval local decls for now
    (Command (Var varId) halt)

-- stack exec mini-mu source.mmu var
