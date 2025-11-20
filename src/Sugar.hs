module Sugar
  (
    desugarProgram,
    desugarDecl,
    desugarExpr,
    desugarCommand,
  )
where

import Syntax
    ( Expr(..),
      VarId,
      Pattern(..),
      Command(..),
      Decl(..),
      Program(..),
      SugarExpr(..),
      HaveBinding(..),
      SugarCommand(..),
      DoThenBinding(Binding),
      MethodDef(..),
      FieldBinding(FieldBinding),
      SugarDecl(..),
      SugarProgram(SugarProgram),
      CommandId )

desugarProgram :: SugarProgram -> Program
desugarProgram (SugarProgram imports decls exports) =
  Program
    { programImports = imports,
      programDecls = map desugarDecl decls,
      programExports = exports
    }

-- | Desugar SugarDecl to Decl | --
desugarDecl :: SugarDecl -> Decl
desugarDecl (FuncDecl name args body) =
  -- fn f x y z = e  =>  f = { (x, y, z) -> desugar(e) }
  let desugaredBody = desugarCommand body
      pat = case args of
        [] -> WildcardPattern
        [x] -> VarPattern x
        xs -> ConsPattern "Tuple" (map VarPattern xs)
   in Decl name (Mu [(pat, desugaredBody)])

desugarDecl (RunDecl cmd) =
  -- run q  =>  _run = desugar(q)
  Decl "main" (Mu [(VarPattern "halt", desugarCommand cmd)])

desugarDecl (DefaultDecl name body) =
  -- x = e  =>  x = desugar(e)
  Decl name (desugarExpr body)

desugarDecl (ModuleDecl name fields methods) =
  -- module obj := field x = e1; field y = e2; Method1(args) -> cmd1; Method2(args) -> cmd2 end
  -- =>
  -- obj = { Method1 args _k -> (e1, e2) . { (_x, _y) -> desugar(cmd1) }
  --       | Method2 args _k -> (e1, e2) . { (_x, _y) -> desugar(cmd2) } }
  let fieldExprs = map (\(FieldBinding _ expr) -> desugarExpr expr) fields
      fieldNames = map (\(FieldBinding fieldName _) -> "_" ++ fieldName) fields
      fieldTuple = Cons "Tuple" fieldExprs
      fieldPattern = case fieldNames of
        [] -> WildcardPattern
        [x] -> VarPattern x
        xs -> ConsPattern "Tuple" (map VarPattern xs)

      -- Desugar each method into a mu clause
      methodClauses = map (desugarMethod fieldTuple fieldPattern) methods
   in Decl name (Mu methodClauses)

-- | Desugar a method definition into a mu clause | --
desugarMethod :: Expr -> Pattern -> MethodDef -> (Pattern, Command)
desugarMethod fieldTuple fieldPattern (MethodDef methodName conts args cmd) =
  let contPatterns = map VarPattern conts
      argPatterns = map VarPattern args
  in 
  if null contPatterns 
    then 
      let fullPattern = ConsPattern methodName $ argPatterns ++ [VarPattern "_k"] -- implicit continuation
          methodBody = Command fieldTuple (Mu [(fieldPattern, desugarCommand cmd)]) in
      (fullPattern, methodBody)
    else 
      let methodBody = Command fieldTuple (Mu [(fieldPattern, desugarCommand cmd)]) in
      (ConsPattern methodName argPatterns, methodBody) -- Full pattern is the argPatterns

-- | Desugar SugarExpr to Expr | --
desugarExpr :: SugarExpr -> Expr
desugarExpr (SugarVar x) = Var x

desugarExpr (NatLit n) = intToPeano n
  where
    intToPeano 0 = Cons "Z" []
    intToPeano i = Cons "S" [intToPeano (i - 1)]

desugarExpr (TupLit es) =
  -- (e1, e2, ..., en)  =>  Tuple e1 e2 ... en
  Cons "Tuple" (map desugarExpr es)

desugarExpr (ListLit es) =
  -- [e1, e2, ..., en]  =>  List:: e1 (List:: e2 (... (List:: en Nil)...))
  foldr
    (\e acc -> Cons "List::" [desugarExpr e, acc])
    (Cons "Nil" [])
    es

desugarExpr (SugarCons c args) =
  -- Foo e1 e2 ... en  =>  Foo (desugar e1) (desugar e2) ... (desugar en)
  Cons c (map desugarExpr args)

desugarExpr (SugarMu cases) =
  -- { p1 -> q1 | p2 -> q2 | ... }  =>  mu [ p1 -> desugar(q1) | p2 -> desugar(q2) | ... ]
  Mu (map (\(p, cmd) -> (p, desugarCommand cmd)) cases)

desugarExpr (SugarDelimExpr cmd) =
  -- < e . k >  =>  delim desugar(e . k)
  DelimExpr (desugarCommand cmd)

desugarExpr (HaveExpr bindings body) =
  -- have bindings in e  =>  substitute bindings in desugar(e)
  desugarHaveExpr bindings (desugarExpr body)

desugarExpr (AppExpr fun explicitConts args) =
  -- f{k1, k2}(x1, x2, k1, k2)  =>  desugared function application
  if null explicitConts
    then
      -- Simple case: f(a, b, c) => { k -> f . (a, b, c, k) }
      let desugaredFun = desugarExpr fun
          desugaredArgs = map desugarExpr args
          newArgs = desugaredArgs ++ [Var "_k"]
       in Mu [(VarPattern "_k", Command desugaredFun (Cons "Tuple" newArgs))]
    else
      -- With explicit continuations: f{k1, k2}(a, k1, b, k2) => { (k1, k2) -> f . (a, k1, b, k2) }
      let desugaredFun = desugarExpr fun
          desugaredArgs = map desugarExpr args
          contsPat = ConsPattern "Tuple" (map toVarPattern explicitConts)
       in Mu [(contsPat, Command desugaredFun (Cons "Tuple" desugaredArgs))]
  where
    toVarPattern (SugarVar x) = VarPattern x
    toVarPattern _ = error "Expected variable in explicit continuation list"

desugarExpr (ThisExpr fieldName) =
  -- this.fieldName  =>  _fieldName
  Var ("_" ++ fieldName)

desugarExpr (MethodCall obj methodName args) =
  -- obj::Method args  =>  { _k -> obj . Method args _k }
  -- This creates a thunk that, when given a continuation, calls the method
  let desugaredObj = desugarExpr obj
      desugaredArgs = map desugarExpr args
      -- The pattern for the method constructor
      methodCall = Cons methodName (desugaredArgs ++ [Var "_k"])
      -- Build: { _k -> obj . Method args _k }
   in Mu [(VarPattern "_k", Command desugaredObj methodCall)]

-- | Desugar SugarCommand to Command | --
desugarCommand :: SugarCommand -> Command
desugarCommand (SugarCommandVar c) = CommandVar c

desugarCommand (LetCommand var e cmd) =
  -- let x <- e in q  =>  desugar(e) . { x -> desugar(q) }
  Command (desugarExpr e) (Mu [(VarPattern var, desugarCommand cmd)])

desugarCommand (LetcCommand var e cmd) =
  -- letcc x <- e in q  =>  { x -> desugar(q) } . desugar(e)
  Command (Mu [(VarPattern var, desugarCommand cmd)]) (desugarExpr e)

desugarCommand (MatchCommand e cases) =
  -- match e with p1 -> q1 | p2 -> q2 | ...  =>  desugar(e) . { p1 -> desugar(q1) | p2 -> desugar(q2) | ... }
  Command (desugarExpr e) (Mu (map (\(p, cmd) -> (p, desugarCommand cmd)) cases))

desugarCommand (PatchCommand e cases) =
  -- dispatch e with p1 -> q1 | p2 -> q2 | ...  =>  { p1 -> desugar(q1) | p2 -> desugar(q2) | ... } . desugar(e)
  Command (Mu (map (\(p, cmd) -> (p, desugarCommand cmd)) cases)) (desugarExpr e)

desugarCommand (DoThenCommand bindings cmd) =
  -- do x <- e1, y <- e2 then q  =>  desugar(e1) . { x -> desugar(e2) . { y -> desugar(q) } }
  foldr
    (\(Binding pat e) acc -> Command (desugarExpr e) (Mu [(pat, acc)]))
    (desugarCommand cmd)
    bindings

desugarCommand (AtCommand fun args) =
  -- f @ a b c  =>  desugar(f) . (desugar(a), desugar(b), desugar(c))
  Command (desugarExpr fun) (Cons "Tuple" (map desugarExpr args))

desugarCommand (CoAtCommand args fun) =
  -- a b c @ 'f  =>  (desugar(a), desugar(b), desugar(c)) . desugar('f)
  Command (Cons "Tuple" (map desugarExpr args)) (desugarExpr fun)

desugarCommand (DotCommand e1 e2) =
  -- e1 . e2  =>  desugar(e1) . desugar(e2)
  Command (desugarExpr e1) (desugarExpr e2)

desugarCommand (ReturnCommand e) =
  -- return e  =>  e . _k
  Command (desugarExpr e) (Var "_k")

-- | Helper function for have expression desugaring | --
desugarHaveExpr :: [HaveBinding] -> Expr -> Expr
desugarHaveExpr [] body = body
desugarHaveExpr (binding : rest) body =
  case binding of
    HaveExprBinding var e ->
      -- Substitute var with desugar(e) in the rest
      let desugaredE = desugarExpr e
       in substExprInExpr var desugaredE (desugarHaveExpr rest body)
    HaveCommandBinding cmdId cmd ->
      -- Substitute cmdId with desugar(cmd) in the rest
      let desugaredCmd = desugarCommand cmd
       in substCmdInExpr cmdId desugaredCmd (desugarHaveExpr rest body)

-- | Substitution helpers | --
substExprInExpr :: VarId -> Expr -> Expr -> Expr
substExprInExpr name e (Var v) = if v == name then e else Var v
substExprInExpr name e (Cons c args) = Cons c (map (substExprInExpr name e) args)
substExprInExpr name e (Mu cases) = Mu (map (\(p, cmd) -> (p, substExprInCmd name e cmd)) cases)
substExprInExpr name e (DelimExpr cmd) = DelimExpr (substExprInCmd name e cmd)

substExprInCmd :: VarId -> Expr -> Command -> Command
substExprInCmd name e (Command e1 e2) = Command (substExprInExpr name e e1) (substExprInExpr name e e2)
substExprInCmd _ _ (CommandVar c) = CommandVar c

substCmdInExpr :: CommandId -> Command -> Expr -> Expr
substCmdInExpr _ _ (Var v) = Var v
substCmdInExpr name cmd (Cons c args) = Cons c (map (substCmdInExpr name cmd) args)
substCmdInExpr name cmd (Mu cases) = Mu (map (\(p, c) -> (p, substCmdInCmd name cmd c)) cases)
substCmdInExpr name cmd (DelimExpr c) = DelimExpr (substCmdInCmd name cmd c)

substCmdInCmd :: CommandId -> Command -> Command -> Command
substCmdInCmd name cmd (Command e1 e2) = Command (substCmdInExpr name cmd e1) (substCmdInExpr name cmd e2)
substCmdInCmd name cmd (CommandVar c) = if c == name then cmd else CommandVar c
