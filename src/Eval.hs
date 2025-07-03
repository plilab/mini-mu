module Eval
  ( eval,
    step,
    evalProgram,
    Config (..),
  )
where

import qualified Data.Map as Map
import Debug.Trace
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
  [ CommandConfig env
      store
      (storeLookupCommand store (envLookup env cmdId))]
step (ValueConfig store cons@(ConsValue {}) (MuValue env clauses)) =
  match env store cons clauses
step (ValueConfig store (MuValue env clauses) cons@(ConsValue {})) =
  match env store cons clauses
step (ValueConfig store v@(MuValue env clauses) cv@(MuValue env' clauses')) =
  match env' store v clauses' ++ match env store cv clauses
-- temporary hack to deal with "Halt"
step (ValueConfig _ cons@(ConsValue _ _) (ConsValue "Halt" [])) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons))]
step (ValueConfig _ (ConsValue "Halt" []) cons@(ConsValue _ _)) = 
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons))]
step (ValueConfig _ (ConsValue {}) (ConsValue {})) =
  [ErrorConfig "Bad type: cannot continue with 2 constructors"]
step (ErrorConfig {}) = []

match :: Env -> Store -> Value -> [(Pattern, Command)] -> [Config]
match _ _ _ [] = []
match env store v ((VarPattern x, cmd) : _) =
  [CommandConfig env' store' cmd]
  where
    (env', store') = envStoreInsert env store x v
match env store v@(ConsValue con args) ((ConsPattern con' params, cmd) : clauses) =
  if con == con'
    then
      let (env', store') =
            trace ("match " ++ show params ++ ":" ++ show args) $
              bind env store params args
       in [CommandConfig env' store' cmd]
    else match env store v clauses
match _ _ (MuValue {}) ((ConsPattern {}, _) : _) =
  [ErrorConfig "bad type, cannot match a Mu"]

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

bind :: Env -> Store -> [VarId] -> [Value] -> (Env, Store)
bind env store [] [] = (env, store)
bind env store (p : ps) (v : vs) = bind env' store' ps vs
  where
    (env', store') = envStoreInsert env store p v
bind _ _ [] (_ : _) = error "Length mismatch at binding"
bind _ _ (_ : _) [] = error "Length mismatch at binding"

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

-- Entrance point for the program evaluation
evalProgram :: Program -> VarId -> Config
evalProgram (Program decls) varId =
  uncurry
    CommandConfig
    (evalDecls initEnv initStore decls)
    (Command (Var varId) halt)

-- stack exec mini-mu source.mmu var