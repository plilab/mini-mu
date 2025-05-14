module Eval (
  eval,
  step,
  evalProgram,
  Config(..),
) where

import Syntax

import qualified Data.Map as Map

evalExpr :: Env -> Expr -> Value
evalExpr env (Var x) = envLookup env x
evalExpr env (CoMu clauses) = CoMuValue env clauses
evalExpr env (Con ident args) = ConValue ident (map (eval env) args)

evalCoExpr :: Env -> CoExpr -> CoValue
evalCoExpr env (CoVar x) = envCoLookup env x
evalCoExpr env (Mu clauses) = MuValue env clauses
evalCoExpr env (CoCon ident args) = CoConValue ident (map (eval env) args)

eval :: Env -> Either Expr CoExpr -> Either Value CoValue
eval env (Left expr) = Left (evalExpr env expr)
eval env (Right coexpr) = Right (evalCoExpr env coexpr)

data Config
  = CommandConfig Env Command -- ρ |- q
  | ValueConfig Value CoValue
  | ErrorConfig String
  deriving (Eq, Show, Ord)

step :: Config -> [Config]
step (CommandConfig env (Command e ce)) =
  [ValueConfig (evalExpr env e) (evalCoExpr env ce)]
step (CommandConfig env (CommandVar cmdId)) =
  [ValueConfig (envLookup env cmdId) (envCoLookup env cmdId)]
step (ValueConfig v@(ConValue {}) (MuValue env clauses)) =
  match env v clauses
step (ValueConfig (CoMuValue env clauses) cv@(CoConValue {})) =
  comatch env cv clauses
step (ValueConfig v@(CoMuValue env clauses) cv@(MuValue env' clauses')) =
  match env' v clauses' ++ comatch env cv clauses
step (ValueConfig (ConValue {}) (CoConValue {})) =
  [ErrorConfig "bad type"]
step (ErrorConfig {}) = []

match :: Env -> Value -> [(Pattern, Command)] -> [Config]
match _ _ [] = []
match env v ((VarPattern x, cmd) : _) =
  [CommandConfig (envInsert env x v) cmd]
match env v@(ConValue con args) ((ConPattern con' params, cmd) : clauses) =
  if con == con'
    then [CommandConfig (bind env params args) cmd]
    else match env v clauses
match  _ (CoMuValue {}) ((ConPattern {}, _) : _) =
  [ErrorConfig "bad type"]

comatch :: Env -> CoValue -> [(CoPattern, Command)] -> [Config]
comatch _ _ [] = []
comatch env v ((CoVarPattern x, cmd) : _) =
  [CommandConfig (envCoInsert env x v) cmd]
comatch env v@(CoConValue con args) ((CoConPattern con' params, cmd) : clauses) =
  if con == con'
    then [CommandConfig (bind env params args) cmd]
    else comatch env v clauses
comatch _ (MuValue {}) ((CoConPattern {}, _) : _) =
  [ErrorConfig "bad type"]

bind :: Env -> [Either VarId CoVarId] -> [Either Value CoValue] -> Env
bind env [] [] = env
bind env (Left p : ps) (Left v : vs) = bind (envInsert env p v) ps vs
bind env (Right p : ps) (Right v : vs) = bind (envCoInsert env p v) ps vs
bind _ (Left _ : _) (Right _ : _) = error "bad type"
bind _ (Right _ : _) (Left _ : _) = error "bad type"
bind _ [] (_ : _) = error "length mismatch"
bind _ (_ : _) [] = error "length mismatch"

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

evalDecls :: [Decl] -> Env
evalDecls ds = env where
  -- TODO: does Map.fromList break the lazy loop?
  env = Env (Map.fromList[(d, evalExpr env e) | Decl d e <- ds]) (Map.fromList [(d, evalCoExpr env e) | CoDecl d e <- ds])

halt :: CoExpr
halt = CoCon "Halt" []

evalProgram :: Program -> VarId -> Config
evalProgram (Program decls) varId = CommandConfig (evalDecls decls) (Command (Var varId) halt)

-- mini-mu source.mmu var
