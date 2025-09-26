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
    go (argValuesAcc, storeAcc) arg =
      (argValuesAcc ++ [argValue], storeAcc')
      where
        (argValue, storeAcc') = eval env storeAcc arg
evalExpr env store (IncompleteCons ident args) = (IncompleteConsValue ident argValues, store')
  where
    (argValues, store') = foldl goIncomplete ([], store) args
    goIncomplete :: ([Either Value HoleValue], Store) -> Either Expr HoleExpr -> ([Either Value HoleValue], Store)
    goIncomplete (argValuesAcc, storeAcc) arg =
      case arg of
        Left expr ->
          (argValuesAcc ++ [Left argValue], storeAcc')
          where
            (argValue, storeAcc') = eval env storeAcc expr
        Right HoleExpr ->
          (argValuesAcc ++ [Right HoleValue], storeAcc)
evalExpr _ _ (IdiomExpr _) =
  error "Illegal use of idiom, it must in command context"
evalExpr _ _ (DerefIdiomExpr _) =
  error "Illegal use of idiom, it must in command context"


evalExprWithCtx :: Env -> Store -> Expr -> Expr -> Either (Value, Store) Config
evalExprWithCtx env store _ (Var x) = Left (storeLookup store (envLookup env x), store)
evalExprWithCtx env store _ (Mu clauses) = Left (MuValue env clauses, store)
evalExprWithCtx env store ctx (Cons ident args) = Left (ConsValue ident argValues, store')
  where
    (argValues, store') = foldl go ([], store) args
    go :: ([Value], Store) -> Expr -> ([Value], Store)
    go (argValuesAcc, storeAcc) arg =
      (argValuesAcc ++ [argValue], storeAcc')
      where
        (argValue, storeAcc') = evalWithCtx env storeAcc ctx arg
evalExprWithCtx env store ctx (IncompleteCons ident args) = Left (IncompleteConsValue ident argValues, store')
  where
    (argValues, store') = foldl goIncomplete ([], store) args
    goIncomplete :: ([Either Value HoleValue], Store) -> Either Expr HoleExpr -> ([Either Value HoleValue], Store)
    goIncomplete (argValuesAcc, storeAcc) arg =
      case arg of
        Left expr ->
          (argValuesAcc ++ [Left argValue], storeAcc')
          where
            (argValue, storeAcc') = evalWithCtx env storeAcc ctx expr
        Right HoleExpr ->
          (argValuesAcc ++ [Right HoleValue], storeAcc)
evalExprWithCtx e s ctx (IdiomExpr cmd@(Command _ _)) =
  Right $ CommandConfigWithCtx e s ctx cmd
evalExprWithCtx _ _ _ (DerefIdiomExpr (Command p c)) =
  error "TODO"
evalExprWithCtx _ _ _ (IdiomExpr (CommandVar _)) =
  error "the command inside idiom should not be a CommandVar"
evalExprWithCtx _ _ _ (DerefIdiomExpr (CommandVar _)) =
  error "the command inside idiom should not be a CommandVar"

eval :: Env -> Store -> Expr -> (Value, Store)
eval env store expr = (value, store')
  where
    (value, store') = evalExpr env store expr

evalWithCtx :: Env -> Store -> Expr -> Expr -> (Value, Store)
evalWithCtx env store ctx expr = case evalExprWithCtx env store ctx expr of
  Left (value, store') -> (value, store')
  Right config -> error $ "Cannot evaluate expression with context to a value directly, got config: " ++ show config
  
      

-- eval env store (Right coexpr) = (Right value, store')
--   where
--     (value, store') = evalCoExpr env store coexpr

step :: Config -> [Config]
step (CommandConfig env store (Command e ce)) =
  let ctxE = Mu [(VarPattern "this", Command (Var "this") ce)] in
  case evalExprWithCtx env store ctxE e of
    Left (value, store') -> 
      let ctxCE = Mu [(VarPattern "this", Command e (Var "this"))] in
      case evalExprWithCtx env store' ctxCE ce of
        Left (coValue, store'') -> [ValueConfig store'' value coValue]
        Right config -> [config]
    Right config -> [config]
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

-- cases for partial applications
step (ValueConfig store iv@(IncompleteConsValue {}) (MuValue env clauses)) =
  match env store iv clauses
step (ValueConfig store (MuValue env clauses) iv@(IncompleteConsValue {})) =
  match env store iv clauses

-- match with context
step (CommandConfigWithCtx env store ctx (Command e ce)) =
  case evalExprWithCtx env store ce e of
    Left (value, store') ->
      case evalExprWithCtx env store' e ce of
        Left (coValue, store'') -> 
          [ValueConfigWithCtx store'' ctx value coValue]
        Right config -> [config]
    Right config -> [config]
step (ValueConfigWithCtx store ctx cons@(ConsValue {}) (MuValue env clauses)) =
  matchWithCtx env store ctx clauses cons clauses
step (ValueConfigWithCtx store ctx (MuValue env clauses) cons@(ConsValue {})) =
  matchWithCtx env store ctx clauses cons clauses
step (ValueConfigWithCtx store ctx v@(MuValue env clauses) cv@(MuValue env' clauses')) =
  matchWithCtx env' store ctx clauses v clauses' ++ matchWithCtx env store ctx clauses cv clauses
step (ValueConfigWithCtx store ctx iv@(IncompleteConsValue {}) (MuValue env clauses)) =
  matchWithCtx env store ctx clauses iv clauses
step (ValueConfigWithCtx store ctx (MuValue env clauses) iv@(IncompleteConsValue {})) =
  matchWithCtx env store ctx clauses iv clauses

-- temporary hack to deal with special constructor "Halt"
step (ValueConfig _ cons@(ConsValue _ _) (ConsValue "Halt" [])) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]
step (ValueConfig _ (ConsValue "Halt" []) cons@(ConsValue _ _)) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]

-- error cases
step (ErrorConfig {}) = []
step _ = [ErrorConfig "Bad type: cannot continue with 2 constructors"]

match :: Env -> Store -> Value -> [(Pattern, Command)] -> [Config]
match _ _ _ [] = [ErrorConfig "No pattern matched"]
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
tryMatch _ _ (IncompleteConsValue {}) _ =
  Nothing -- Cannot match IncompleteConsValue without context


matchWithCtx :: Env -> Store -> Expr -> [(Pattern, Command)] -> Value -> [(Pattern, Command)] -> [Config]
matchWithCtx _ _ _ _ _ [] = [ErrorConfig "No pattern matched"]
matchWithCtx env store ctx initPat v ((pat, cmd@(Command e ce)) : clauses) =
  case tryMatchComp env store v pat of
    -- VERY IMPORTANT: if complete match, the program diverges from the original path
    -- could be very useful
    Just (env', store') -> [CommandConfig env' store' cmd]
    Nothing -> 
      case tryMatchIncomp v pat of
        -- reconstruct the original CommandConfig from the context
        Just (cons, unmatchedPats, exprAcc) ->
          -- create a new Mu expresion that eliminates all the matched variables
          let oldMu = Mu initPat in
          let newMu = Mu [(ConsPattern cons unmatchedPats, Command oldMu (Cons cons exprAcc))] in
          [CommandConfig env store (Command newMu ctx)]
        Nothing -> match env store v clauses
matchWithCtx env store _ _ v ((pat, cmd@(CommandVar _)) : clauses) =
  case tryMatchComp env store v pat of
    Just (env', store') -> [CommandConfig env' store' cmd]
    Nothing -> match env store v clauses -- we dont support partial match for CommandVar

-- Core pattern matching logic

-- match all complete case
tryMatchComp :: Env -> Store -> Value -> Pattern -> Maybe (Env, Store)
tryMatchComp = tryMatch
-- {Ap a b k -> ...} becomes { Ap b k -> 1 b k . {Ap a b k -> ...} }
-- only matching incomplete cases
tryMatchIncomp :: Value -> Pattern -> Maybe (ConsId, [Pattern], [Expr])
-- add support for wildcards
tryMatchIncomp (ConsValue con args) (WildcardPattern) =
  error "todo"
tryMatchIncomp (IncompleteConsValue con args) (WildcardPattern) =
  error "todo"
-- Add support for complete cons inside incomplete cons e.g. Ap _ (Ap 1 2) _
tryMatchIncomp (ConsValue con args) (ConsPattern con' pats)
  | con == con' && length args == length pats = do
      (unmatched, acc) <- foldM matchArg ([], []) (zip (map Left args) pats)
      return (con, unmatched, acc)
  | otherwise = Nothing
  where
    matchArg :: ([Pattern], [Expr]) -> (Either Value HoleValue, Pattern) -> Maybe ([Pattern], [Expr])
    matchArg (unmatched, acc) (Left v, pat) = do
      (innerCons, innerUnmatched, innerAcc) <- tryMatchIncomp v pat
      -- construct new pattern for this inner pattern
      case innerUnmatched of
        [] -> do -- if inner is a complete match, then remove this pattern is matched, otherwise it is unmatched
          return (unmatched, acc ++ [Cons innerCons innerAcc]) -- unmatched unchanged
          -- and new acc is appended with the inner value
          -- e.g. Ap a (Ap b c) k matching with Ap _ (Ap 1 2) _, acc is: [a, (Ap 1 2), k]
        _ -> do
          let newPat = ConsPattern innerCons innerUnmatched -- keep the unmatched inner patterns
          return (unmatched ++ [newPat], acc ++ [Cons innerCons innerAcc])
          -- e.g. Ap a (Ap b c) k matching with Ap _ (Ap 1 _) _, acc is: [a, (Ap 1 c), k]
    matchArg (unmatched, acc) (Right _, pat) =
      case pat of
        WildcardPattern -> Just (unmatched ++ [pat], acc ++ [Var "*"]) -- Holes meets wildcard
        VarPattern v -> Just (unmatched ++ [pat], acc ++ [Var v]) -- Holes becomes variable
        ConsPattern {} -> Just (unmatched ++ [pat], acc ++ [consPatternToExpr pat])
      -- e.g. Ap a (Ap b c) k matching with Ap 1 _ _, acc is: [1, bc, k]
tryMatchIncomp (IncompleteConsValue con args) (ConsPattern con' pats)
  | con == con' && length args == length pats = do
      (unmatched, acc) <- foldM matchIncompArg ([], []) (zip args pats)
      return (con, unmatched, acc)
  | otherwise = Nothing
  where
    matchIncompArg :: ([Pattern], [Expr]) -> (Either Value HoleValue, Pattern) -> Maybe ([Pattern], [Expr])
    matchIncompArg (unmatched, acc) (Left v, pat) = do
      (innerCons, innerUnmatched, innerAcc) <- tryMatchIncomp v pat
      -- construct new pattern for this inner pattern
      case innerUnmatched of
        [] -> do -- if inner is a complete match, then remove this pattern is matched, otherwise it is unmatched
          return (unmatched, acc ++ [Cons innerCons innerAcc]) -- unmatched unchanged
          -- and new acc is appended with the inner value
          -- e.g. Ap a (Ap b c) k matching with Ap _ (Ap 1 2) _, acc is: [a, (Ap 1 2), k]
        _ -> do
          let newPat = ConsPattern innerCons innerUnmatched -- keep the unmatched inner patterns
          return (unmatched ++ [newPat], acc ++ [Cons innerCons innerAcc])
          -- e.g. Ap a (Ap b c) k matching with Ap _ (Ap 1 _) _, acc is: [a, (Ap 1 c), k]
    matchIncompArg (unmatched, acc) (Right _, pat) =
      case pat of
        WildcardPattern -> Just (unmatched ++ [pat], acc ++ [Var "*"]) -- Holes meets wildcard
        VarPattern v -> Just (unmatched ++ [pat], acc ++ [Var v]) -- Holes becomes variable
        ConsPattern {} -> Just (unmatched ++ [pat], acc ++ [consPatternToExpr pat])
      -- e.g. Ap a (Ap b c) k matching with Ap 1 _ _, acc is: [1, bc, k]
tryMatchIncomp _ _ = Nothing

consPatternToExpr :: Pattern -> Expr
consPatternToExpr (ConsPattern cid pats) = Cons cid (map consPatternToExpr pats)
consPatternToExpr (VarPattern v) = Var v
consPatternToExpr WildcardPattern = Var "" -- represent wildcard as a variable with empty name

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = return acc
foldM f acc (x : xs) = do
  acc' <- f acc x
  foldM f acc' xs

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
    (evalDecls initEnv initStore decls) -- Still just eval local decls for now
    (Command (Var varId) halt)

-- stack exec mini-mu source.mmu var
