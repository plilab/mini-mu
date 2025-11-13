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
simplifyExpr (Mu cases) = 
  let
    this = "_thisfocus" 
    newcases = denest (map (Data.Bifunctor.second simplifyCmd) cases) (Var this)
    matchcmd = Command (Var this) (Mu newcases)
  in
  Mu [ (VarPattern this, matchcmd) ]
simplifyExpr (Cons c args) = Cons c (map simplifyExpr args)
simplifyExpr (Var v) = Var v

simplifyCmd :: Command -> Command
simplifyCmd (Command e1 e2) = Command (simplifyExpr e1) (simplifyExpr e2)
simplifyCmd (CommandVar c) = CommandVar c

-- converts all nested pattern matching into single-level pattern matching
denest :: [(Pattern, Command)] -> Expr -> [(Pattern, Command)]
denest [] _ = []
-- in these 2 cases, the var/wildcard will match anything,
-- so we ignore the rest of the cases
denest ((WildcardPattern, cmd):_) _ = [(WildcardPattern, cmd)]
denest ((VarPattern v, cmd):_) _ = [(VarPattern v, cmd)]
denest ((cons, cmd):rest) thisExpr =
  if isSimplePattern cons then
    (cons, cmd) : denest rest thisExpr
  else
    let
      rest' = denest rest thisExpr
      fallback = Mu rest' -- if the current match fails, try the rest
      failcmd = Command thisExpr fallback
      nofallback = null rest'
      (newPattern, bindings, currNewFreshIdx) = extractPattern cons 0
    in
      (newPattern, bindByOne bindings currNewFreshIdx nofallback cmd failcmd) : rest' 

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
bindByOne [] _ _ successCmd _ = successCmd
bindByOne ((expr, pattern):rest) currIdx nofallback successCmd failureCmd =
  let failBranch = (WildcardPattern, failureCmd) in
  if isSimplePattern pattern then
    let 
      innerCmd = bindByOne rest currIdx nofallback successCmd failureCmd
      muBody = if nofallback then [(pattern, innerCmd)] else [(pattern, innerCmd), failBranch]
    in
    Command expr (Mu muBody)
  else
    let
      (newPattern, bindings, nextIdx) = extractPattern pattern currIdx
      newBindings = bindings ++ rest
      innerCmd = bindByOne newBindings nextIdx nofallback successCmd failureCmd
      muBody = if nofallback then [(newPattern, innerCmd)] else [(newPattern, innerCmd), failBranch] 
    in
    Command expr (Mu muBody)