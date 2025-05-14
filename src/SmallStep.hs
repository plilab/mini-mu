module SmallStep where

-- import Syntax

-- -- maybe put both value and covalue in the same map of an environment
-- -- <q>
-- -- <qv>
-- -- <e>
-- -- <ce>
-- -- <v>
-- -- <cv>
-- data Config
--   = CommandConfig Env Command -- ρ |- q
--   | ExprConfig Env Expr [Frame] CommandFrame --  ρ |- e . f ... . <_ | e/v >
--   | CoExprConfig Env CoExpr [Frame] CommandFrame --  ρ |- e . f ... . <_ | e/v >
--   | ValueConfig Env Value [Frame] CommandFrame -- ρ |- v . f ... . <_ | e/v >
--   | CoValueConfig Env CoValue [Frame] CommandFrame -- ρ |- v . f ... . <_ | e/v >
--   | CommandValueConfig Value CoValue
--   | ErrorConfig String

-- data CommandFrame
--   = CommandFrame (Either CoExpr CoValue) -- <_ | e> | <_ | v>
--   | CoCommandFrame (Either Expr Value) -- <e | _> | <v | _>
-- data Frame = Frame (Either ConId CoConId) [Either Value CoValue] [Either Expr CoExpr] -- C v v _ e e

-- -- data CoFrame = CoFrame CoConId [Either Value CoValue] [Either Expr CoExpr] -- C v v _ e e

-- -- GraphViz - Dot

-- -- stack

-- step :: Config -> [Config]
-- step (CommandConfig env (Command e k)) = [ExprConfig env e [] (CommandFrame (Left k)), CoExprConfig env k [] (CoCommandFrame (Left e))]

-- step (ExprConfig env (Var id) fs cf) = [ValueConfig env (envLookup env id) fs cf]
-- step (ExprConfig env (CoMu clauses) fs cf) = [ValueConfig env (CoMuValue env clauses) fs cf] -- value!
-- step (ExprConfig env (Con id []) fs cf) = [ValueConfig env (ConValue id []) fs cf] -- value!
-- step (ExprConfig env (Con id (Left e : args)) fs cf) = [ExprConfig env e (Frame (Left id) [] args : fs) cf]
-- step (ExprConfig env (Con id (Right ce : args)) fs cf) = [CoExprConfig env ce (Frame (Left id) [] args : fs) cf]

-- step (CoExprConfig {}) = [ErrorConfig "unimplemented expr config"]

-- step (ValueConfig env v [] (CommandFrame (Left ce))) = [CoExprConfig env ce [] (CoCommandFrame (Right v))]
-- step (ValueConfig env v [] (CommandFrame (Right cv))) = [CommandValueConfig v cv]
-- step (ValueConfig env v [] (CoCommandFrame (Left ce))) = [CoExprConfig env ce [] (CoCommandFrame (Right v))]
-- step (ValueConfig env v [] (CoCommandFrame (Right cv))) = [CommandValueConfig v cv]
-- step (ValueConfig env v (Frame id vs [] : fs) cf) = [ValueConfig env (ConValue id (vs ++ [Left v])) fs cf] -- value!
-- step (ValueConfig env v (Frame id vs (Left e : es) : fs) cf) = [ExprConfig env e (Frame id (vs ++ [Left v]) es : fs) cf]
-- step (ValueConfig env v (Frame id vs (Right ce : es) : fs) cf) = [CoExprConfig env ce (Frame id (vs ++ [Left v]) es : fs) cf]

-- step (CoValueConfig {}) = [ErrorConfig "unimplemented"]

-- step (CommandValueConfig (ConValue id vs) (MuValue env clauses)) = match id vs env clauses
-- step (CommandValueConfig (CoMuValue env clauses) (CoConValue id vs)) = comatch id vs env clauses
-- step (CommandValueConfig v@(CoMuValue env clauses) cv@(MuValue env' clauses')) = match v env' clauses' ++ comatch cv env clauses
-- step (CommandValueConfig (ConValue {}) (CoConValue {})) = [ErrorConfig "bad type"]

-- match :: Value -> Env -> [(Pattern, Command)] -> [Config]
-- match v env [] = []
-- match v env ((VarPattern x, q) : _) = CommandConfig env' q where
--   env' = Map.set env x (ConValue id args)
-- match (ConValue id args) env ((ConPattern id' params, q) : _) =
--   if id == id' then CommandConfig (bind env' params args) q else match id args env clauses
-- match _ env (ConPattern {}) = [ErrorConfig "missmatch"]

-- bind :: Env -> [Either VarId CoVarId] -> [Either Value CoValue] -> Env
-- bind env [] [] = env
-- bind env (Left p : ps) (Left v : vs) = bind (Map.set env p v) ps vs
-- bind env (Right p : ps) (Right v : vs) = bind (Map.set env p v) ps vs
-- bind env (Left _ : _) (Right _ : _) = error "bad"
-- bind env (Right _ : _) (Left _ : _) = error "bad"

-- v . < _ | e > . ....



