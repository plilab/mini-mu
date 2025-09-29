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
import Pretty (prettyTopLevelValue, renderPretty, prettyConfig)
import Syntax 
  (
    Env(..), 
    Store(..),
    Expr(..),
    Value(..),
    Pattern(..),
    Program(..),
    Decl(..),
    Command(..),
    Context(..),
    Config(..),
    VarId,
    Addr(..),
    envStoreInsert,
    envStoreMerge,
    envLookup,
    storeLookup,
    storeLookupCommand,
  )
-- import Debug.Trace (trace)

evalExpr :: Env -> Store -> Expr -> (Value, Store)
evalExpr env store (Var x) = (storeLookup store (envLookup env x), store)
evalExpr env store (Mu clauses) = (MuValue env clauses, store)
evalExpr _ store Hole = (HoleValue, store)
evalExpr env store (Cons ident args) = (ConsValue ident argValues, store')
  where
    (argValues, store') = foldl go ([], store) args
    go :: ([Value], Store) -> Expr -> ([Value], Store)
    go (argValuesAcc, storeAcc) arg =
      (argValuesAcc ++ [argValue], storeAcc')
      where
        (argValue, storeAcc') = eval env storeAcc arg
evalExpr _ _ (IdiomExpr _) =
  error "Illegal use of idiom, it must in command context"


evalExprWithCtx :: Env -> Store -> Context -> Expr -> Either (Value, Store) Config
evalExprWithCtx env store _ (Var x) = Left (storeLookup store (envLookup env x), store)
evalExprWithCtx env store _ (Mu clauses) = Left (MuValue env clauses, store)
evalExprWithCtx env store ctx (Cons ident args) = Left (ConsValue ident argValues, store')
  where
    (argValues, store') = foldl go ([], store) args
    go :: ([Value], Store) -> Expr -> ([Value], Store)
    go (argValuesAcc, storeAcc) arg =
      case evalWithCtx env storeAcc ctx arg of
        Left (argValue, storeAcc') -> (argValuesAcc ++ [argValue], storeAcc')
        Right config -> let (argValue, storeAcc') = evalConfig config in (argValuesAcc ++ [argValue], storeAcc')

evalExprWithCtx _ s _ Hole =
  Left (HoleValue, s)
evalExprWithCtx e s ctx (IdiomExpr cmd@(Command _ _)) =
  Right $ CommandConfigWithCtx e s ctx cmd
evalExprWithCtx _ _ _ (IdiomExpr (CommandVar _)) =
  error "the command inside idiom should not be a CommandVar"

evalConfig :: Config -> (Value, Store)
evalConfig cfg@(CommandConfigWithCtx {}) =
  case stepUntil cfg of  -- TODO: this is temporarys
    CommandConfig env store (Command e _) -> evalExpr env store e
    _ -> error $ show cfg
    where stepUntil c@(CommandConfig {}) = c
          stepUntil c = case step c of
                          [c'] -> stepUntil c'
                          [] -> error "stuck"
                          _ -> error "nondeterministic step"
evalConfig _ = error "TODO"

eval :: Env -> Store -> Expr -> (Value, Store)
eval env store expr = (value, store')
  where
    (value, store') = evalExpr env store expr

evalWithCtx :: Env -> Store -> Context -> Expr -> Either (Value, Store) Config
evalWithCtx env store ctx expr = case evalExprWithCtx env store ctx expr of
  Left (value, store') -> Left (value, store')
  Right config -> Right config

-- eval env store (Right coexpr) = (Right value, store')
--   where
--     (value, store') = evalCoExpr env store coexpr

step :: Config -> [Config]
-- Command configs
step (CommandConfig env store (Command e ce)) =
  let e' = Mu [(VarPattern "this", Command (Var "this") ce)] in
  case evalExprWithCtx env store (CommandContext TopContext env store e') e of
    Left (value, store') -> 
      let ce' = Mu [(VarPattern "this", Command e (Var "this"))] in
      case evalExprWithCtx env store' (CommandContext TopContext env store' ce') ce of
        Left (coValue, store'') -> [ValueConfig store'' value coValue]
        Right config -> [config]
    Right config -> [config]
step (CommandConfig env store (CommandVar cmdId)) =
  [ CommandConfig
      env
      store
      (storeLookupCommand store (envLookup env cmdId))
  ]

-- Value configs
step (ValueConfig store cons@(ConsValue {}) (MuValue env clauses)) =
  match env store cons clauses
step (ValueConfig store (MuValue env clauses) cons@(ConsValue {})) =
  match env store cons clauses
step (ValueConfig store v@(MuValue env clauses) cv@(MuValue env' clauses')) =
  match env' store v clauses' ++ match env store cv clauses
step (ValueConfig _ HoleValue _) =
  [ErrorConfig "Hole cannot be used as standalone value, it must be wrapped within a constructor"]
step (ValueConfig _ _ HoleValue) =
  [ErrorConfig "Hole cannot be used as standalone value, it must be wrapped within a constructor"]

-- temporary hack to deal with special constructor "Halt"
step (ValueConfig _ cons@(ConsValue _ _) (ConsValue "Halt" [])) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]
step (ValueConfig _ (ConsValue "Halt" []) cons@(ConsValue _ _)) =
  [ErrorConfig ("Halt with result: " ++ renderPretty (prettyTopLevelValue cons False))]

-- 2 constructors cannot continue
step (ValueConfig _ (ConsValue {}) (ConsValue {})) = 
  [ErrorConfig "Bad type: cannot continue with 2 constructors"]

-- Command with context
step (CommandConfigWithCtx env store ctx (Command e ce)) =
  let e' = Mu [(VarPattern "this", Command (Var "this") ce)] in
  -- difference is that we nesting the contexts here
  case evalExprWithCtx env store (CommandContext ctx env store e') e of
    Left (value, store') ->
      let ce' = Mu [(VarPattern "this", Command e (Var "this"))] in
      case evalExprWithCtx env store' (CommandContext ctx env store' ce') ce of
        Left (coValue, store'') -> 
          [ValueConfigWithCtx store'' ctx value coValue]
        Right config -> [config]
    Right config -> [config]

step (CommandConfigWithCtx env store ctx (CommandVar cmdId)) =
  [ CommandConfigWithCtx
      env
      store
      ctx
      (storeLookupCommand store (envLookup env cmdId))
  ]

-- Value with context
step (ValueConfigWithCtx store ctx cons@(ConsValue {}) (MuValue env clauses)) =
  matchWithCtx env store ctx cons clauses
step (ValueConfigWithCtx store ctx (MuValue env clauses) cons@(ConsValue {})) =
  matchWithCtx env store ctx cons clauses
step (ValueConfigWithCtx store ctx v@(MuValue env clauses) cv@(MuValue env' clauses')) =
  matchWithCtx env' store ctx v clauses' ++ matchWithCtx env store ctx cv clauses
step (ValueConfigWithCtx _ _ HoleValue _) =
  [ErrorConfig "Hole cannot be used as standalone value, it must be wrapped within a constructor"]
step (ValueConfigWithCtx _ _ _ HoleValue) =
  [ErrorConfig "Hole cannot be used as standalone value, it must be wrapped within a constructor"]
step (ValueConfigWithCtx _ _ (ConsValue {}) (ConsValue {})) = 
  [ErrorConfig "Bad type: cannot continue with 2 constructors"]

-- Deal with constructors in small step semantics
step (ConstructorConfig env store ctx id [] vals) =
  error "todo"
step (ConstructorConfig env store ctx _ (e : es) vals) =
  error "td"
-- stop when error
step (ErrorConfig {}) = []

match :: Env -> Store -> Value -> [(Pattern, Command)] -> [Config]
match _ _ _ [] = [ErrorConfig "No pattern matched"]
match env store v ((pat, cmd) : clauses) =
  case tryMatch env store v pat of
    Just (env', store') -> [CommandConfig env' store' cmd]
    Nothing -> match env store v clauses

matchWithCtx :: Env -> Store -> Context -> Value -> [(Pattern, Command)] -> [Config]
matchWithCtx _ _ _ _ [] = [ErrorConfig "No pattern matched"]
matchWithCtx env store ctx v ((pat, cmd) : clauses) =
  case tryMatch env store v pat of
    -- VERY IMPORTANT: if complete match, the program diverges from the original path
    -- could be useful
    Just (env', store') -> [CommandConfig env' store' cmd]
    Nothing ->
      case tryPartialMatch env store v pat of
        -- reconstruct the original CommandConfig from the context
        Just (env'', store'') ->
          let shrinkedPat = shrinkPattern v pat id in
            case shrinkedPat of
              Just spat ->
                let newClause = Mu [(spat, cmd)] in
                  case ctx of
                    CommandContext upper ctxEnv ctxStore ctxExpr ->
                      case upper of
                        TopContext ->
                          let (env''', store''') = envStoreMerge env'' store'' ctxEnv ctxStore in
                          [CommandConfig env''' store''' $ Command newClause ctxExpr]
                        CommandContext upper' _ _ _ ->
                          let (env''', store''') = envStoreMerge env'' store'' ctxEnv ctxStore in
                          [
                            CommandConfigWithCtx
                            env''' store''' upper'
                            $ Command newClause ctxExpr
                          ]
                        _ -> error "todo"
                    _ -> error "Impossible"
              Nothing -> error "Impossible"
        Nothing -> match env store v clauses

-- Core pattern matching logic
tryMatch :: Env -> Store -> Value -> Pattern -> Maybe (Env, Store)
tryMatch _ _ HoleValue _ =
  Nothing -- deal with hole in the tryPartialMatch
-- otherwise
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

tryPartialMatch :: Env -> Store -> Value -> Pattern -> Maybe (Env, Store)
tryPartialMatch env store HoleValue _ = 
  Just (env, store) -- Hole always matches
tryPartialMatch env store _ WildcardPattern = 
  Just (env, store) -- Wildcard always matches
tryPartialMatch env store v (VarPattern x) =
  Just (envStoreInsert env store x v) -- Bind variable
tryPartialMatch env store (ConsValue con args) (ConsPattern con' pats)
  | con == con' && length args == length pats =
      foldM matchArg (env, store) (zip args pats)
  | otherwise = Nothing
  where
    matchArg (env', store') (arg, pat) =
      tryPartialMatch env' store' arg pat
tryPartialMatch _ _ (MuValue {}) (ConsPattern {}) = Nothing -- Cannot match MuValue with constructor pattern

-- cps shrinking pattern
-- value = Ap 1 (Ap _ 2) _, pattern = Ap x (Ap y z) w
-- shrinkPattern value pattern cont = Ap (Ap y) w
type NullablePattern = Maybe Pattern

shrinkPattern :: Value -> Pattern -> (NullablePattern -> NullablePattern) -> NullablePattern
shrinkPattern HoleValue pat cont = 
  cont (Just pat) -- base case: hole will through the hole pattern it matches into cont

shrinkPattern _ WildcardPattern cont =  -- wildcard always matches
  cont Nothing -- matched, skip
shrinkPattern _ (VarPattern _) cont = -- var always matches
  cont Nothing -- matched, skip

shrinkPattern (ConsValue con args) (ConsPattern con' pats) cont
  | con == con' && length args == length pats =
      processArgs
        args
        pats
        []
        ( \subpats ->
            if null subpats
              then cont Nothing -- if all subpatterns vanished
              else cont (Just (ConsPattern con' subpats))
        )
  | otherwise = error "Cannot shrink non-matching pattern"
  where
    -- processArgs traverses args/pats left-to-right,
    -- collecting surviving subpatterns
    processArgs :: [Value] -> [Pattern] -> [Pattern] -> ([Pattern] -> NullablePattern) -> NullablePattern
    processArgs [] [] acc k = k (reverse acc)
    processArgs (arg : args') (pat : pats') acc k =
      shrinkPattern arg pat $ \resPat ->
        case resPat of
          Nothing -> processArgs args' pats' acc k
          Just pat' -> processArgs args' pats' (pat' : acc) k
    processArgs _ _ _ _ = error "Impossible" -- should not happen due to length check above
shrinkPattern (MuValue {}) (ConsPattern {}) _ =
  error "Cannot shrink non-matching pattern"

-- Ap a (Ap b c) d ~ Ap 1 (Ap 2 _) _ 
-- { Ap a (Ap b c) d -> ... } => { Ap (Ap c) d -> ... }. We will return a shrinked pattern as additional



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
