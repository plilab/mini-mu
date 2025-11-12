module Fresh (fresh) where
import Syntax
import qualified Data.Map as Map
import Data.List (isPrefixOf)

-- | Alpha-renaming | --

-- | Entry point for renaming a program to use fresh variable names | --
fresh :: Program -> Program
fresh (Program imports decls exports) =
  let (decls', _) = runFresh (mapMFresh freshDecl decls) (Map.empty, 0)
  in Program imports decls' exports

-- | Renaming environment: maps original variable names to their renamed versions | --
type Renaming = (Map.Map VarId VarId, Int)

-- | Fresh monad: tracks renaming environment and a counter for generating unique names | --
type FreshM a = Renaming -> (a, Renaming)

runFresh :: FreshM a -> Renaming -> (a, Renaming)
runFresh = id

returnFresh :: a -> FreshM a
returnFresh x st = (x, st)

bindFresh :: FreshM a -> (a -> FreshM b) -> FreshM b
bindFresh ma f st =
  let (a, st') = ma st
  in f a st'

-- | Generate a fresh name for a binding occurrence
-- This creates a NEW binding but does NOT extend the environment
-- The environment extension happens later in withBinding/withBindings
generateFreshName :: VarId -> FreshM VarId
generateFreshName var (env, counter) =
  if "_" `isPrefixOf` var
  then
    let newName = var ++ show counter
        counter' = counter + 1
    in (newName, (env, counter'))
  else (var, (env, counter))

-- | Look up the renamed version of a variable (use occurrence)
-- This looks up an EXISTING binding in the environment
lookupVarName :: VarId -> FreshM VarId
lookupVarName var (env, counter) =
  if "_" `isPrefixOf` var
  then case Map.lookup var env of
    Just renamed -> (renamed, (env, counter))
    Nothing -> error $ "Unbound variable: " ++ var
  else (var, (env, counter))

-- | Extend environment with a new binding (for patterns)
withBinding :: VarId -> VarId -> FreshM a -> FreshM a
withBinding oldName newName action (env, counter) =
  let env' = Map.insert oldName newName env
      (result, (_, counter')) = action (env', counter)
  in (result, (env, counter'))  -- Restore original environment after action

-- Fresh instances for each syntactic category
freshDecl :: Decl -> FreshM Decl
freshDecl (Decl varId expr) =
  -- Top-level declarations bind variables globally
  bindFresh (generateFreshName varId) $ \varId' ->
  bindFresh (withBinding varId varId' (freshExpr expr)) $ \expr' ->
  returnFresh (Decl varId' expr')

freshExpr :: Expr -> FreshM Expr
freshExpr (Var varId) =
  -- Variables are use occurrences - look them up
  bindFresh (lookupVarName varId) $ \varId' ->
  returnFresh (Var varId')
freshExpr (Cons consId exprs) =
  bindFresh (mapMFresh freshExpr exprs) $ \exprs' ->
  returnFresh (Cons consId exprs')
freshExpr (Mu branches) =
  -- Each branch introduces new bindings
  bindFresh (mapMFresh freshBranch branches) $ \branches' ->
  returnFresh (Mu branches')

-- Process a pattern and its body with proper scoping
freshBranch :: (Pattern, Command) -> FreshM (Pattern, Command)
freshBranch (pat, cmd) =
  -- Process pattern to generate fresh names for binders
  bindFresh (freshPatternBindings pat) $ \(pat', bindings) ->
  -- Process the command body with the new bindings in scope
  bindFresh (withBindings bindings (freshCommand cmd)) $ \cmd' ->
  returnFresh (pat', cmd')

-- Extract bindings from a pattern and generate fresh names
freshPatternBindings :: Pattern -> FreshM (Pattern, [(VarId, VarId)])
freshPatternBindings (ConsPattern consId pats) =
  bindFresh (mapMFresh freshPatternBindings pats) $ \results ->
  let (pats', bindingsList) = unzip results
      allBindings = concat bindingsList
  in returnFresh (ConsPattern consId pats', allBindings)
freshPatternBindings (VarPattern varId) =
  bindFresh (generateFreshName varId) $ \varId' ->
  returnFresh (VarPattern varId', [(varId, varId')])
freshPatternBindings WildcardPattern =
  returnFresh (WildcardPattern, [])

-- Apply multiple bindings to an action
withBindings :: [(VarId, VarId)] -> FreshM a -> FreshM a
withBindings [] action = action
withBindings ((old, new):rest) action =
  withBinding old new (withBindings rest action)

freshCommand :: Command -> FreshM Command
freshCommand (Command expr1 expr2) =
  bindFresh (freshExpr expr1) $ \expr1' ->
  bindFresh (freshExpr expr2) $ \expr2' ->
  returnFresh (Command expr1' expr2')
freshCommand (CommandVar cmdId) = returnFresh (CommandVar cmdId)

-- Helper for monadic map
mapMFresh :: (a -> FreshM b) -> [a] -> FreshM [b]
mapMFresh _ [] = returnFresh []
mapMFresh f (x:xs) =
  bindFresh (f x) $ \y ->
  bindFresh (mapMFresh f xs) $ \ys ->
  returnFresh (y:ys)
