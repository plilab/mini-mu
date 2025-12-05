module Focus (focus) where
import Syntax
import qualified Data.Bifunctor

-- | target mu pattern matching to convert nested pattern matching to single-level pattern matching | --
focus :: Program -> Program
focus (Program imports decls exports) =
  Program imports (map simplifyDecl decls) exports
  where
    simplifyDecl (Decl valId expr) =
      Decl valId (simplifyExpr expr) 

simplifyExpr :: Expr -> Expr
simplifyExpr e = simplifyExprAux e id

simplifyExprAux :: Expr -> (Expr -> a) -> a
simplifyExprAux (Mu cases) k = 
  let
    this = "_thisfocus"
    oldcases = map (Data.Bifunctor.second (`simplifyCmdAux` id)) cases
  in
    if all (isSimplePattern . fst) oldcases 
    then
      k (Mu oldcases)
    else
      let
        newcases = denest oldcases (Var this)
        matchcmd = Command (Var this) (Mu newcases)
      in
      k (Mu [ (VarPattern this, matchcmd) ])
simplifyExprAux (Cons c args) k =
  let
    go [] acc k' = k' (Cons c (reverse acc))
    go (a:as) acc k' =
      simplifyExprAux a (\a' -> go as (a':acc) k')
  in
  go args [] k
simplifyExprAux (Var v) k = k (Var v)
simplifyExprAux (DelimExpr cmd) k =
  simplifyCmdAux cmd (k . DelimExpr)

simplifyCmdAux :: Command -> (Command -> a) -> a
simplifyCmdAux (Command e1 e2) k =
  simplifyExprAux e1 (\e1' ->
    simplifyExprAux e2 (k . Command e1'))
simplifyCmdAux (CommandVar c) k = k (CommandVar c)

-- converts nested pattern matches into single-level pattern matches
denest :: [(Pattern, Command)] -> Expr -> [(Pattern, Command)]
denest cases thisExpr = denestAux cases []
  where
    denestAux :: [(Pattern, Command)] -> [(Pattern, Command)] -> [(Pattern, Command)]
    denestAux [] acc = reverse acc
    denestAux ((WildcardPattern, cmd):_) acc = reverse ((WildcardPattern, cmd):acc)
    denestAux ((VarPattern v, cmd):_) acc = reverse ((VarPattern v, cmd):acc)
    denestAux ((cons, cmd):rest) acc =
      if isSimplePattern cons then
        denestAux rest ((cons, cmd):acc)
      else
        let
          fallback = if null rest then Mu [] else Mu (denest rest thisExpr)
          failcmd = Command thisExpr fallback
          nofallback = null rest
          (newPattern, bindings, currNewFreshIdx) = extractPattern cons 0
          newCmd = bindByOne bindings currNewFreshIdx nofallback cmd failcmd
        in
          denestAux rest ((newPattern, newCmd):acc)

isSimplePattern :: Pattern -> Bool
isSimplePattern WildcardPattern = True
isSimplePattern (VarPattern _) = True
-- if any sub-pattern is a cons pattern, then not simple
isSimplePattern (ConsPattern _ ps) = 
  all notCons ps
  where
    notCons (ConsPattern {}) = False
    notCons _ = True

-- given a possibly nested pattern, create a non-nested version with fresh variables,
-- and a list of bindings from the fresh variables to the original pattern structure
-- (1 layer denesting.)
extractPattern :: Pattern -> Int -> (Pattern, [(Expr, Pattern)], Int)
extractPattern WildcardPattern idx = (VarPattern "_thiswildcard", [], idx)
extractPattern (VarPattern v) idx = (VarPattern v, [], idx)
extractPattern (ConsPattern c pats) idx =
  -- a reduce over pats
  let
    (newPats, bindingsList, idx') = foldl go ([], [], idx) pats
  in
    (ConsPattern c newPats, bindingsList, idx')
  where
    go :: ([Pattern], [(Expr, Pattern)], Int) -> Pattern -> ([Pattern], [(Expr, Pattern)], Int)
    go (accPats, accBindings, idx') p =
      case p of
        WildcardPattern -> (accPats ++ [VarPattern ("_wildcard" ++ show idx')], accBindings, idx' + 1)
        VarPattern v -> (accPats ++ [VarPattern v], accBindings, idx')
        ConsPattern c' pats' ->
          let
            freshVar = "_patternvar" ++ show idx'
          in
            (accPats ++ [VarPattern freshVar], accBindings ++ [(Var freshVar, ConsPattern c' pats')], idx' + 1)

-- given a list of variables and their associated patterns,
-- the current number of fresh variables used,
-- whether there is no fallback (i.e., this is the last pattern to try),
-- a success command and a failure command, create a command that
-- binds each variable to its pattern one by one, and on failure goes to the failure command
bindByOne :: [(Expr, Pattern)] -> Int -> Bool -> Command -> Command -> Command
bindByOne topBindings currIdx nofallback successCmd failureCmd =
  go topBindings currIdx successCmd
  where
    go :: [(Expr, Pattern)] -> Int -> Command -> Command
    go [] _ accCmd = accCmd
    go ((expr, pat):rest) idx accCmd =
      let failBranch = (WildcardPattern, failureCmd) in
      if isSimplePattern pat then
        let 
          muBody = if nofallback && null rest
                     then [(pat, accCmd)]
                     else [(pat, accCmd), failBranch]
          newCmd = Command expr (Mu muBody)
        in
          go rest idx newCmd
      else
        let
          (newPattern, newBindings, nextIdx) = extractPattern pat idx
          -- we will need to reevaluate this pattern with the simplified version
          newTodo = newBindings ++ (expr, newPattern):rest
        in
          go newTodo nextIdx accCmd
