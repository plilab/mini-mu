module Eval (
  eval,
  step,
  evalProgram,
  Config(..),
) where

import Syntax

import qualified Data.Map as Map
import Debug.Trace

evalExpr :: Env -> Store -> Expr -> (Value, Store)
evalExpr env store (Var x) = (storeLookup store (envLookup env x), store)
evalExpr env store (CoMu clauses) = (CoMuValue env clauses, store)
evalExpr env store (Cons ident args) = (ConsValue ident argValues, store') 
  where
    (argValues, store') = foldl go ([], store) args
    go :: ([Either Value CoValue], Store) -> Either Expr CoExpr -> ([Either Value CoValue], Store)
    go (argValuesAccum, storeAccum) arg = (argValuesAccum ++ [argValue], storeAccum') 
      where
        (argValue, storeAccum') = eval env storeAccum arg

evalCoExpr :: Env -> Store -> CoExpr -> (CoValue, Store)
evalCoExpr env store (CoVar x) = (storeCoLookup store (envCoLookup env x), store)
evalCoExpr env store (Mu clauses) = (MuValue env clauses, store)
evalCoExpr env store (CoCons ident args) = (CoConsValue ident argValues, store')
  where
    (argValues, store') = foldl go ([], store) args
    go :: ([Either Value CoValue], Store) -> Either Expr CoExpr -> ([Either Value CoValue], Store)
    go (argValuesAccum, storeAccum) arg = (argValuesAccum ++ [argValue], storeAccum')
      where 
        (argValue, storeAccum') = eval env storeAccum arg

eval :: Env -> Store -> Either Expr CoExpr -> (Either Value CoValue, Store)
eval env store (Left expr) = (Left value, store') where
  (value, store') = evalExpr env store expr
eval env store (Right coexpr) = (Right value, store') where
  (value, store') = evalCoExpr env store coexpr

-- let-and-set
-- CESK: Expr Env Store CoValue
-- Us: Expr Env Store CoExpr / Value (no-Env) Store CoValue
data Config
  = CommandConfig Env Store Command -- ρ |- q
  | ValueConfig Store Value CoValue
  | ErrorConfig String
  deriving (Eq, Show, Ord)

step :: Config -> [Config]
step (CommandConfig env store (Command e ce)) =
  [ValueConfig store'' value coValue] where
    (value, store') = evalExpr env store e
    (coValue, store'') = evalCoExpr env store' ce
step (CommandConfig env store (CommandVar cmdId)) =
  [ValueConfig store
    (storeLookup store (envLookup env cmdId))
    (storeCoLookup store (envCoLookup env cmdId))]
step (ValueConfig store v@(ConsValue {}) (MuValue env clauses)) =
  match env store v clauses
step (ValueConfig store (CoMuValue env clauses) cv@(CoConsValue {})) =
  comatch env store cv clauses
step (ValueConfig store v@(CoMuValue env clauses) cv@(MuValue env' clauses')) =
  match env' store v clauses' ++ comatch env store cv clauses
-- temporary hack to deal with "Halt"
step (ValueConfig _ cons@(ConsValue _ _) (CoConsValue "Halt" [])) =
  [ErrorConfig ("Halt with result: " ++ show cons)]
step (ValueConfig _ (ConsValue {}) (CoConsValue {})) =
  [ErrorConfig "bad type: cannot continue with 2 constructors"]
step (ErrorConfig {}) = []

match :: Env -> Store -> Value -> [(Pattern, Command)] -> [Config]
match _ _ _ [] = []
match env store v ((VarPattern x, cmd) : _) = [CommandConfig env' store' cmd] where
  (env', store') = envStoreInsert env store x v
match env store v@(ConsValue con args) ((ConsPattern con' params, cmd) : clauses) =
  if con == con'
    then let (env', store') = trace ("match " ++ show params ++ ":" ++ show args) $ bind env store params args in
          [CommandConfig env' store' cmd]
    else match env store v clauses
match _ _ (CoMuValue {}) ((ConsPattern {}, _) : _) =
  [ErrorConfig "bad type"]

comatch :: Env -> Store -> CoValue -> [(CoPattern, Command)] -> [Config]
comatch _ _ _ [] = []
comatch env store v ((CoVarPattern x, cmd) : _) = [CommandConfig env' store' cmd] where
  (env', store') = envStoreCoInsert env store x v
comatch env store v@(CoConsValue con args) ((CoConsPattern con' params, cmd) : clauses) =
  if con == con'
    then let (env', store') = trace (show "comatch") $ bind env store params args in
          [CommandConfig env' store' cmd]
    else comatch env store v clauses
comatch _ _ (MuValue {}) ((CoConsPattern {}, _) : _) =
  [ErrorConfig "bad type"]

bind :: Env -> Store -> [Either VarId CoVarId] -> [Either Value CoValue] -> (Env, Store)
bind env store [] [] = (env, store)
bind env store (Left p : ps) (Left v : vs) = bind env' store' ps vs where
  (env', store') = envStoreInsert env store p v
bind env store (Right p : ps) (Right v : vs) = bind env' store' ps vs where
  (env', store') = envStoreCoInsert env store p v
bind _ _ (Left _ : _) (Right _ : _) = error "bad type"
bind _ _ (Right _ : _) (Left _ : _) = error "bad type"
bind _ _ [] (_ : _) = error "length mismatch"
bind _ _ (_ : _) [] = error "length mismatch"

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
evalDecls env store ds = (env', store') where
  (env', store') = foldl go (env, store) ds
  go :: (Env, Store) -> Decl -> (Env, Store)
  go (envAccum, storeAccum) (Decl d e) =
    envStoreInsert envAccum storeAccum' d value where
      (value, storeAccum') = evalExpr env' storeAccum e
  go (envAccum, storeAccum) (CoDecl d e) =
    envStoreCoInsert envAccum storeAccum' d covalue where
      (covalue, storeAccum') = evalCoExpr env' storeAccum e

-- cesk machine

initEnv :: Env
initEnv = Env Map.empty Map.empty

initStore :: Store
initStore = Store (Addr 0) (CoAddr 0) Map.empty Map.empty

halt :: CoExpr
halt = CoCons "Halt" []

evalProgram :: Program -> VarId -> Config
evalProgram (Program decls) varId = uncurry CommandConfig (evalDecls initEnv initStore decls) (Command (Var varId) halt)

-- mini-mu source.mmu var