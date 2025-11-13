module Focus (focus) where
import Syntax

-- we handle the static focusing case here,
-- where we decide which argument to focus on
-- based on the input program.
-- the hope is that this will prevent us from
-- matching a Mu against a pattern.
-- as an example, consider the following mu-expr:
-- { (S (S n), x) -> command1 }
-- using cutlet style syntax,
-- this should be focused such that it becomes:
-- { (a, b) -> 
--   match a with 
--   | S c ->
--   match c with
--   | S d -> 
--   let n <- d in
--   let x <- b in
--   command1
-- }
-- 
-- in general, we only need to focus when arguments are matched,
-- ie when mu-closures are being used in pattern matching.
-- more advanced matches may be tricky.
-- the focusing should only be done on exprs - leave everythin else alone!

focus :: Program -> Program
focus p = Program {
    programDecls = map focusDecl (programDecls p),
    programExports = programExports p,
    programImports = programImports p -- the module loader invokes the parser,
                                      -- so import focusing has already been resolved here
}

focusDecl :: Decl -> Decl
focusDecl (Decl varId expr) =
    Decl varId (focusExpr expr)

focusExpr :: Expr -> Expr
focusExpr e@(Var {}) = e
focusExpr e@(Cons {}) = e
focusExpr (Mu branches) =
    case branches of
        [] -> Mu branches
        -- fast optimization:
        -- if the top branch is a variable pattern/wildcard,
        -- don't bother with the rest
        (VarPattern _, _) : _ -> Mu branches
        (WildcardPattern, _) : _  -> Mu branches
        _ -> Mu (focusBranches branches)

focusCommand :: Command -> Command
focusCommand (Command e1 e2) =
    Command (focusExpr e1) (focusExpr e2)
focusCommand c@(CommandVar {}) = c

-- given a set of clauses, group them such that
-- they are grouped if their outermost pattern constructor is the same.
-- then for each group, we focus on the unique component.
-- this is responsible for transforming
-- {
--    Foo (Bar x) y -> Cmd1
-- |  Foo (Bar z) w -> Cmd2
-- |  Foo a (BarBaz b) -> Cmd3
-- }

-- into 

-- {
--     Foo _a _b ->
--       match _a with
--       | Bar x ->
--         let y <- _b in
--         Cmd1 
--       | Bar z ->
--         let w <- _b in
--         Cmd2
--       | a ->
--         match _b with
--         | BarBaz b -> Cmd3
-- }

data CommonBranch = 
    Tree {
    cbConstructor :: ConsId,
    cbFreshVars :: Int,
    subBranches :: [(Pattern, Command)]
    } deriving (Show, Eq)
    | Leaf {
    cbBranches :: [(Pattern, Command)]
    }
   

focusBranches :: [(Pattern, Command)] -> [(Pattern, Command)]



