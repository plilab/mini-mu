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

-- evalCoExpr :: Env -> Store -> CoExpr -> (CoValue, Store)
-- evalCoExpr env store (CoVar x) = (storeCoLookup store (envCoLookup env x), store)
-- evalCoExpr env store (Mu clauses) = (MuValue env clauses, store)
-- evalCoExpr env store (CoCons ident args) = (CoConsValue ident argValues, store')
--   where
--     (argValues, store') = foldl go ([], store) args
--     go :: ([Either Value CoValue], Store) -> Either Expr CoExpr -> ([Either Value CoValue], Store)
--     go (argValuesAccum, storeAccum) arg =
--       (argValuesAccum ++ [argValue], storeAccum')
--       where
--         (argValue, storeAccum') = eval env storeAccum arg

eval :: Env -> Store -> Expr -> (Value, Store)
eval env store expr = (value, store')
  where
    (value, store') = evalExpr env store expr

-- eval env store (Right coexpr) = (Right value, store')
--   where
--     (value, store') = evalCoExpr env store coexpr

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
step (ValueConfig store cons@(ConsValue {}) (MuValue env clauses)) =
  match env store cons clauses
step (ValueConfig store (MuValue env clauses) cons@(ConsValue {})) =
  match env store cons clauses
step (ValueConfig store v@(MuValue env clauses) cv@(MuValue env' clauses')) =
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
  Just (env, store)  -- Wildcard always matches

tryMatch env store v (VarPattern x) = 
  Just (envStoreInsert env store x v)  -- Bind variable

tryMatch env store (ConsValue con args) (ConsPattern con' pats)
  | con == con' && length args == length pats = 
      foldM matchArg (env, store) (zip args pats)
  | otherwise = Nothing
  where
    matchArg (env', store') (arg, pat) = 
      tryMatch env' store' arg pat

tryMatch _ _ (MuValue {}) (ConsPattern {}) = 
  Nothing  -- Cannot match MuValue with constructor pattern

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = return acc
foldM f acc (x:xs) = do
  acc' <- f acc x
  foldM f acc' xs

-- comatch :: Env -> Store -> CoValue -> [(CoPattern, Command)] -> [Config]
-- comatch _ _ _ [] = []
-- comatch env store v ((CoVarPattern x, cmd) : _) =
--   [CommandConfig env' store' cmd]
--   where
--     (env', store') = envStoreCoInsert env store x v
-- comatch env store v@(CoConsValue con args) ((CoConsPattern con' params, cmd) : clauses) =
--   if con == con'
--     then
--       let (env', store') =
--             trace (show "comatch") $ bind env store params args
--        in [CommandConfig env' store' cmd]
--     else comatch env store v clauses
-- comatch _ _ (MuValue {}) ((CoConsPattern {}, _) : _) =
--   [ErrorConfig "bad type, cannot match MuValue with CoConsPattern"]

-- bind :: Env -> Store -> [VarId] -> [Value] -> (Env, Store)
-- bind env store [] [] = (env, store)
-- bind env store (p : ps) (v : vs) = bind env' store' ps vs
--   where
--     (env', store') = envStoreInsert env store p v
-- bind _ _ [] (_ : _) = error "Length mismatch at binding"
-- bind _ _ (_ : _) [] = error "Length mismatch at binding"

-- type Graph = Map Config (Set Config)
-- stepAll :: Config -> Map Config (Set Config)
-- stepAll config =
--   for each c in step config do:
--     add config -> c to map
--     if c was not previously in map:
--       stepAll c
--   return map

-- zero = Z
-- one = S(zero)

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
    (evalDecls initEnv initStore decls)  -- Still just eval local decls for now
    (Command (Var varId) halt)

-- stack exec mini-mu source.mmu var