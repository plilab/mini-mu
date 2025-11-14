module Pretty
  ( prettyConfig,
    prettyCommand,
    prettyExpr,
    -- prettyCoExpr,
    prettyPattern,
    -- prettyCoPattern,
    renderPretty,
    -- prettyTopLevelEitherValue,
    prettyTopLevelValue,
    -- prettyTopLevelCoValue,
    prettyProgram,
    prettySugaredProgram,
  )
where

import qualified Data.Map as Map
import Prettyprinter
    ( Doc,
      (<+>),
      defaultLayoutOptions,
      hang,
      hsep,
      indent,
      layoutPretty,
      line,
      punctuate,
      vsep,
      braces,
      brackets,
      comma,
      parens,
      pipe,
      space,
      Pretty(pretty) )
import Prettyprinter.Render.String ( renderString )
import Syntax
    ( Env(..),
      Store(..),
      Expr(..),
      Value(..),
      VarId,
      Addr(..),
      Pattern(..),
      Command(..),
      Config(..),
      Decl(..),
      Program(Program),
      ImportDecl(..),
      SugarExpr(..),
      HaveBinding(..),
      SugarCommand(..),
      DoThenBinding(..),
      MethodDef(..),
      FieldBinding(..),
      SugarDecl(..),
      SugarProgram(SugarProgram),
      CommandId, OneHoleContext (Context) )

-- | Pretty print a configuration.
prettyConfig :: Config -> Bool -> Doc ann
prettyConfig (CommandConfig env store command) _show =
  pretty "<Command Config>"
    <> line
    <> prettyEnv env _show
    <> line
    <> prettyStore store _show
    <> line
    <> pretty "COMMAND:"
    <+> prettyCommand command
prettyConfig (ValueConfig store value value') _show =
  pretty "<Value Config>"
    <> line
    <> prettyStore store _show
    <> line
    <> pretty "VALUE:"
    <+> prettyTopLevelValue value _show
    <> line
    <> pretty "COVALUE:"
    <+> prettyTopLevelValue value' _show
prettyConfig (ConsEvalConfig env store frames currentExpr ce) _show =
  pretty "<ConsEval Config>"
    <> line
    <> prettyEnv env _show
    <> line
    <> prettyStore store _show
    <> line
    <> pretty "FRAMES:"
    <+> pretty (show frames)
    <> line
    <> pretty "CURRENT EXPR:"
    <+> prettyExpr currentExpr
    <> line
    <> pretty "CONTINUATION:"
    <+> prettyExpr ce
prettyConfig (CoConsEvalConfig env store value frames currentExpr) _show =
  pretty "<ConsCoEval Config>"
    <> line
    <> prettyEnv env _show
    <> line
    <> prettyStore store _show
    <> line
    <> pretty "VALUE:"
    <+> prettyTopLevelValue value _show
    <> line
    <> pretty "FRAMES:"
    <+> pretty (show frames)
    <> line
    <> pretty "CURRENT COEXPR:"
    <+> prettyExpr currentExpr
prettyConfig (DelimConfig delimConfig ctx@(Context _ _)) _show =
  pretty "<Delim Config>"
    <> line
    <> pretty "CURRENT CONFIG:"
    <> line
    <> indent 2 (prettyConfig delimConfig _show)
    <> line
    <> pretty "ONE-HOLE CONTEXT:"
    <+> prettyContext ctx
    <> line
prettyConfig (ErrorConfig string) _ =
  pretty "<Message> " <> pretty string

prettyContext :: OneHoleContext -> Doc ann
prettyContext (Context varId config) =
  pretty "Context with hole variable:" <+> pretty varId
    <> line
    <> pretty "Containing config:"
    <> line
    <> indent 2 (prettyConfig config True)


-- | Pretty Printing Programs | --
prettyProgram :: Program -> Doc ann
prettyProgram (Program imports decls exports) =
  pretty "Program"
    <+> braces
      ( line
          <> indent 2 (prettyImports imports)
          <> line
          <> indent 2 (prettyDecls decls)
          <> line
          <> indent 2 (prettyExports exports)
          <> line )

-- | Pretty print import declarations | --
prettyImports :: [ImportDecl] -> Doc ann
prettyImports [] = mempty
prettyImports (ImportDecl name vars : is) =
  pretty "import"
  <+> pretty name 
  <+> pretty "(" 
  <> hsep (punctuate comma (map pretty vars)) 
  <> pretty ")"
  <> line
  <> prettyImports is

-- | Pretty print declarations | --
prettyDecls :: [Decl] -> Doc ann
prettyDecls [] = mempty
prettyDecls ds =
  mconcat [prettyDecl d | d <- ds]

-- | Pretty print the main expression (exports) | --
prettyExports :: [VarId] -> Doc ann
prettyExports es = pretty "export" <+> hsep (punctuate comma (map pretty es))

-- | Pretty printing a declaration | --
prettyDecl :: Decl -> Doc ann
prettyDecl (Decl varId expr) =
  pretty varId
    <+> pretty ":="
    <+> prettyExpr expr
    <> line
    <> line

-- | Pretty Printing Commands | --
prettyCommand :: Command -> Doc ann
prettyCommand (Command e k) =
  space
    <> prettyTopLevelExpr e
    <+> pretty "."
    <+> prettyTopLevelExpr k
    <> space
prettyCommand (CommandVar x) = pretty x

-- | Pretty Printing Expressions | --

-- | Pretty print a top-level Expression | --
prettyTopLevelExpr :: Expr -> Doc ann
prettyTopLevelExpr (Var x) = pretty x
prettyTopLevelExpr (Cons "Z" []) = pretty "0"
prettyTopLevelExpr add1@(Cons "S" _) =
  maybe (prettyNatExprFallback add1) pretty (prettyNatExpr add1)
prettyTopLevelExpr (Cons "Nil" []) = pretty "[]"
prettyTopLevelExpr li@(Cons "List::" _) =
  prettyListExpr li
prettyTopLevelExpr tup@(Cons "Tuple" _) =
  prettyTupleExpr tup
prettyTopLevelExpr (Cons con args) =
  pretty con <+> hsep (map prettyExpr args)
prettyTopLevelExpr mu@(Mu _) =
  prettyMuExprAux mu
prettyTopLevelExpr (DelimExpr cmd) =
  pretty "<" <> line <> indent 2 (prettyCommand cmd) <> line <> pretty ">"

-- | Pretty print an Expression | --
prettyExpr :: Expr -> Doc ann
prettyExpr (Var x) = pretty x
prettyExpr (Cons "Z" []) = pretty "0"
prettyExpr add1@(Cons "S" _) =
  maybe (prettyNatExprFallback add1) pretty (prettyNatExpr add1)
prettyExpr (Cons "Nil" []) = pretty "[]"
prettyExpr listExpr@(Cons "List::" _) = prettyListExpr listExpr
prettyExpr tupExpr@(Cons "Tuple" _) = prettyTupleExpr tupExpr
prettyExpr (Cons c args) =
  case args of
    [] -> pretty c
    _ -> pretty "(" <> pretty c <+> hsep (map prettyExpr args) <> pretty ")"
prettyExpr (Mu cases) = prettyMuExprAux (Mu cases)
prettyExpr (DelimExpr cmd) =
  pretty "<" <> line <> indent 2 (prettyCommand cmd) <> line <> pretty ">"

-- | Convert Peano number expressions to Integer if possible | --
prettyNatExpr :: Expr -> Maybe Integer
prettyNatExpr (Cons "Z" []) = Just 0
prettyNatExpr (Cons "S" [e]) = (+ 1) <$> prettyNatExpr e
prettyNatExpr _ = Nothing

-- | Fallback pretty printing for Peano numbers | --
prettyNatExprFallback :: Expr -> Doc ann
prettyNatExprFallback (Cons "Z" []) = pretty "0"
prettyNatExprFallback (Cons "S" n) = pretty "S" <+> prettyNatExprFallback (head n)
prettyNatExprFallback e = prettyExpr e

-- | Helper function to pretty print List:: as [e1, e2, ..., en] | --
prettyListExpr :: Expr -> Doc ann
prettyListExpr expr =
  case collectListElements expr [] of
    Just elements -> brackets (hsep (punctuate comma (map prettyExpr elements)))
    Nothing -> prettyExprFallback expr
  where
    collectListElements :: Expr -> [Expr] -> Maybe [Expr]
    collectListElements (Cons "Nil" []) acc = Just (reverse acc)
    collectListElements (Cons "List::" [e, rest]) acc =
      collectListElements rest (e : acc)
    collectListElements _ _ = Nothing

    prettyExprFallback :: Expr -> Doc ann
    prettyExprFallback (Cons c args) =
      case args of
        [] -> pretty c
        _ -> pretty "(" <> pretty c <+> hsep (map prettyExpr args) <> pretty ")"
    prettyExprFallback e = prettyExpr e

-- | Helper function to pretty print Tuple as (e1, e2, ..., en) | --
prettyTupleExpr :: Expr -> Doc ann
prettyTupleExpr (Cons "Tuple" args) =
  parens (hsep (punctuate comma (map prettyExpr args)))
prettyTupleExpr e = prettyExpr e

-- | Helper function to pretty print Mu expressions | --
prettyMuExprAux :: Expr -> Doc ann
prettyMuExprAux (Mu cases) =
  braces (hang (-1) (prettyCases cases))
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyBranch c
        <> mconcat [line <> pipe <> space <> prettyBranch c' | c' <- cs]
prettyMuExprAux _ = error "Expected a Mu expression"


-- | Pretty Printing Values | --

-- | Pretty print a top-level Value | --
prettyTopLevelValue :: Value -> Bool -> Doc ann
prettyTopLevelValue (ConsValue "Z" []) _ = pretty "0"
prettyTopLevelValue (ConsValue "S" [v]) _ =
  pretty (prettyNatValue (ConsValue "S" [v]))
prettyTopLevelValue (ConsValue "Nil" []) _ = pretty "[]"
prettyTopLevelValue listVal@(ConsValue "List::" _) showEnv =
  prettyListValue listVal showEnv
prettyTopLevelValue tupVal@(ConsValue "Tuple" _) showEnv =
  prettyTupleValue tupVal showEnv
prettyTopLevelValue (ConsValue con args) showEnv =
  pretty con <+> hsep (map (`prettyValue` showEnv) args)
prettyTopLevelValue mu@(MuValue _ _) showEnv =
  prettyMuValueAux mu showEnv

-- | Pretty print a Value | --
prettyValue :: Value -> Bool -> Doc ann
prettyValue (ConsValue "Z" []) _ = pretty "0"
prettyValue (ConsValue "S" [v]) _ =
  pretty (prettyNatValue (ConsValue "S" [v]))
prettyValue (ConsValue "Nil" []) _ = pretty "[]"
prettyValue listVal@(ConsValue "List::" _) showEnv = prettyListValue listVal showEnv
prettyValue tupVal@(ConsValue "Tuple" _) showEnv = prettyTupleValue tupVal showEnv
prettyValue (ConsValue con args) showEnv =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map (`prettyValue` showEnv) args) <> pretty ")"
prettyValue mu@(MuValue _ _) showEnv =
  prettyMuValueAux mu showEnv

-- | Helper function to convert Peano numbers to integers | --
prettyNatValue :: Value -> Integer
prettyNatValue (ConsValue "Z" []) = 0
prettyNatValue (ConsValue "S" [v]) = 1 + prettyNatValue v
prettyNatValue _ = error "Not a Peano number" -- safe cause there is no variable names in values

-- | Helper function to pretty print List:: values as [v1, v2, ..., vn] | --
prettyListValue :: Value -> Bool -> Doc ann
prettyListValue val showEnv =
  case collectListValueElements val [] of
    Just elements -> brackets (hsep (punctuate comma (map (`prettyValue` showEnv) elements)))
    Nothing -> prettyValueFallback val showEnv
  where
    collectListValueElements :: Value -> [Value] -> Maybe [Value]
    collectListValueElements (ConsValue "Nil" []) acc = Just (reverse acc)
    collectListValueElements (ConsValue "List::" [v, rest]) acc =
      collectListValueElements rest (v : acc)
    collectListValueElements _ _ = Nothing

    prettyValueFallback :: Value -> Bool -> Doc ann
    prettyValueFallback (ConsValue con args) se =
      case args of
        [] -> pretty con
        _ -> pretty "(" <> pretty con <+> hsep (map (`prettyValue` se) args) <> pretty ")"
    prettyValueFallback v se = prettyValue v se

-- | Helper function to pretty print Tuple values as (v1, v2, ..., vn) | --
prettyTupleValue :: Value -> Bool -> Doc ann
prettyTupleValue (ConsValue "Tuple" args) showEnv =
  parens (hsep (punctuate comma (map (`prettyValue` showEnv) args)))
prettyTupleValue v showEnv = prettyValue v showEnv

-- | Helper function to pretty print Mu values | --
prettyMuValueAux :: Value -> Bool -> Doc ann
prettyMuValueAux (MuValue env cases) showEnv =
  pretty "μ-Closure"
    <+> braces
      ( line
          <> indent
            2
            (prettyEnv env showEnv <+> line <> braces (hang (-1) (prettyCases cases)))
          <> line
      )
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyBranch c
        <> mconcat [line <> pipe <> space <> prettyBranch c' | c' <- cs]
prettyMuValueAux _ _ = error "Expected a MuValue"

-- | Pretty print a branch (pattern and command) | --
prettyBranch :: (Pattern, Command) -> Doc ann
prettyBranch (pat, cmd) =
  prettyPattern pat <+> pretty "->" <> line <> indent 2 (prettyCommand cmd)

-- | Pretty print a pattern | --
prettyPattern :: Pattern -> Doc ann
prettyPattern (ConsPattern "Nil" []) = pretty "[]"
prettyPattern listPat@(ConsPattern "List::" _) = prettyListPattern listPat
prettyPattern tupPat@(ConsPattern "Tuple" _) = prettyTuplePattern tupPat
prettyPattern (ConsPattern con args) =
  case args of
    [] -> pretty con
    _ -> pretty con <+> hsep (map prettyPattern args)
prettyPattern (VarPattern x) = pretty x
prettyPattern WildcardPattern = pretty "_"

-- | Helper function to pretty print List:: patterns as [p1, p2, ..., pn]
prettyListPattern :: Pattern -> Doc ann
prettyListPattern pat =
  case collectListPatternElements pat [] of
    Just elements -> brackets (hsep (punctuate comma (map prettyPattern elements)))
    Nothing -> prettyPatternFallback pat
  where
    collectListPatternElements :: Pattern -> [Pattern] -> Maybe [Pattern]
    collectListPatternElements (ConsPattern "Nil" []) acc = Just (reverse acc)
    collectListPatternElements (ConsPattern "List::" [p, rest]) acc =
      collectListPatternElements rest (p : acc)
    collectListPatternElements _ _ = Nothing

    prettyPatternFallback :: Pattern -> Doc ann
    prettyPatternFallback (ConsPattern con args) =
      case args of
        [] -> pretty con
        _ -> pretty con <+> hsep (map prettyPattern args)
    prettyPatternFallback p = prettyPattern p

-- | Helper function to pretty print Tuple patterns as (p1, p2, ..., pn)
prettyTuplePattern :: Pattern -> Doc ann
prettyTuplePattern (ConsPattern "Tuple" args) =
  parens (hsep (punctuate comma (map prettyPattern args)))
prettyTuplePattern p = prettyPattern p

-- | Env and Store Pretty Printing | --

-- | Pretty print an environment | --
prettyEnv :: Env -> Bool -> Doc ann
prettyEnv (Env varMap cmdMap) True =
  pretty "Environment"
    <+> braces
      ( line
          <> indent 2 (pretty "Variables:" <+> prettyVarMap varMap)
          <> line
          <> indent 2 (pretty "Commands:" <+> prettyCommandMap cmdMap)
          <> line
      )
    <> line
  where
    prettyVarMap :: Map.Map VarId Addr -> Doc ann
    prettyVarMap m
      | Map.null m = pretty "{}"
      | otherwise =
          braces
            ( line
                <> indent
                  2
                  ( vsep
                      [ pretty v
                          <+> pretty "↦"
                          <+> prettyAddr addr
                        | (v, addr) <- Map.toList m
                      ]
                  )
                <> line
            )
    prettyCommandMap :: Map.Map CommandId Addr -> Doc ann
    prettyCommandMap m
      | Map.null m = pretty "{}"
      | otherwise =
          braces
            ( line
                <> indent
                  2
                  ( vsep
                      [ pretty c
                          <+> pretty "↦"
                          <+> prettyAddr addr
                        | (c, addr) <- Map.toList m
                      ]
                  )
                <> line
            )
prettyEnv _ False = pretty "{ Environment }"

-- | Pretty print an address | --
prettyAddr :: Addr -> Doc ann
prettyAddr (Addr n) = pretty "#" <> pretty n

-- | Pretty print a store | --
prettyStore :: Store -> Bool -> Doc ann
prettyStore (Store addr cmdAddr counter valMap cmdMap) True =
  pretty "Store"
    <+> braces
      ( line
          <> indent
            2
            ( vsep
                [ pretty "Next Address:" <+> prettyAddr addr,
                  pretty "Next Command Address:" <+> prettyAddr cmdAddr,
                  pretty "Subst Counter:" <+> pretty counter,
                  line,
                  pretty "Values:" <+> prettyValueMap valMap,
                  pretty "Commands:" <+> prettyCommandMap cmdMap
                ]
            )
          <> line
      )
    <> line
  where
    prettyValueMap :: Map.Map Addr Value -> Doc ann
    prettyValueMap m
      | Map.null m = pretty "{}"
      | otherwise =
          braces
            ( line
                <> indent
                  2
                  ( vsep
                      [ prettyAddr addr'
                          <+> pretty "↦"
                          <+> prettyTopLevelValue val True
                        | (addr', val) <- Map.toList m
                      ]
                  )
                <> line
            )
    prettyCommandMap :: Map.Map Addr Command -> Doc ann
    prettyCommandMap m
      | Map.null m = pretty "{}"
      | otherwise =
          braces
            ( line
                <> indent
                  2
                  ( vsep
                      [ prettyAddr addr'
                          <+> pretty "↦"
                          <+> prettyCommand cmd
                        | (addr', cmd) <- Map.toList m
                      ]
                  )
                <> line
            )
prettyStore _ False = pretty "{ Store }"

-- | Pretty Print Sugared Program | --

-- | Pretty print a Sugared Program | --
prettySugaredProgram :: SugarProgram -> Doc ann
prettySugaredProgram (SugarProgram imports decls exports) =
  pretty "Sugared Program"
    <+> braces
      ( line
          <> indent 2 (prettyImports imports)
          <> line
          <> indent 2 (prettySugarDecls decls)
          <> line
          <> indent 2 (prettyExports exports)
          <> line
      )

-- | Pretty print sugared declarations | --
prettySugarDecls :: [SugarDecl] -> Doc ann
prettySugarDecls [] = mempty
prettySugarDecls sdecls =
  mconcat [prettySugarDecl d | d <- sdecls]

-- | Pretty print a sugared declaration | --
prettySugarDecl :: SugarDecl -> Doc ann
prettySugarDecl (FuncDecl name args cmd) =
  pretty "fn"
    <+> pretty name
    <+> hsep (map pretty args)
    <+> pretty ":="
    <> line
    <> indent 2 (prettySugarCommand cmd)
    <> pretty ";" 
    <> line
    <> line
prettySugarDecl (RunDecl cmd) =
  pretty "run"
    <+> prettySugarCommand cmd
    <> pretty ";"
    <> line
prettySugarDecl (DefaultDecl name expr) =
  pretty name
    <+> pretty ":="
    <+> prettyTopLevelSugarExpr expr
    <> pretty ";"
    <> line
prettySugarDecl (ModuleDecl name fields methods) =
  pretty "module"
    <+> pretty name
    <+> pretty ":="
    <> line
    <> indent 2 (vsep (map prettyFieldBinding fields))
    <> line <> line
    <> indent 2 (vsep (map prettyMethodDef methods))
    <> line
    <> pretty "end"
    <> pretty ";"
    <> line
    <> line

-- | Pretty print a field binding | --
prettyFieldBinding :: FieldBinding -> Doc ann
prettyFieldBinding (FieldBinding fieldName expr) =
      pretty "field"
        <+> pretty fieldName
        <+> pretty "="
        <+> prettyTopLevelSugarExpr expr

-- | Pretty print a method definition | --
prettyMethodDef :: MethodDef -> Doc ann
prettyMethodDef (MethodDef methodName conts args cmd) =
  pretty methodName
    <> hsep (map pretty conts)
    <> parens (hsep (punctuate comma (map pretty args)))
    <+> pretty "->"
    <> line
    <> indent 2 (prettySugarCommand cmd)
    <> line

-- | Pretty print a sugared command | --
prettySugarCommand :: SugarCommand -> Doc ann
prettySugarCommand (LetCommand var expr cmd) =
  pretty "let"
    <+> pretty var
    <+> pretty "="
    <+> prettyTopLevelSugarExpr expr
    <+> pretty "in"
    <> line
    <> prettySugarCommand cmd
prettySugarCommand (LetcCommand var expr cmd) =
  pretty "letc"
    <+> pretty var
    <+> pretty "="
    <+> prettyTopLevelSugarExpr expr
    <+> pretty "in"
    <> line
    <> prettySugarCommand cmd
prettySugarCommand (MatchCommand expr branches) =
  pretty "match"
    <+> prettyTopLevelSugarExpr expr
    <+> pretty "with"
    <> line
    <> indent 2 (prettySugarBranches branches)
prettySugarCommand (PatchCommand expr branches) =
  pretty "patch"
    <+> prettyTopLevelSugarExpr expr
    <+> pretty "with"
    <> line
    <> indent 2 (prettySugarBranches branches)
prettySugarCommand (DoThenCommand bindings cmd) =
  pretty "do"
    <> line
    <> indent 2 (vsep (map prettyDoThenBinding bindings))
    <> line
    <> pretty "then"
    <> line
    <> prettySugarCommand cmd
prettySugarCommand (AtCommand expr args) =
  prettyTopLevelSugarExpr expr
    <+> pretty "@"
    <+> hsep (map prettySugarExpr args) -- args are not top-level
prettySugarCommand (CoAtCommand args expr) =
  hsep (map prettySugarExpr args) -- args are not top-level
    <+> pretty "@"
    <+> prettyTopLevelSugarExpr expr
prettySugarCommand (DotCommand expr1 expr2) =
  prettyTopLevelSugarExpr expr1
    <+> pretty "."
    <+> prettyTopLevelSugarExpr expr2
prettySugarCommand (ReturnCommand expr) =
  pretty "return"
    <+> prettyTopLevelSugarExpr expr
prettySugarCommand (SugarCommandVar cmdId) =
  pretty cmdId

-- | Pretty print sugared branches | --
prettySugarBranches :: [(Pattern, SugarCommand)] -> Doc ann
prettySugarBranches [] = mempty
prettySugarBranches (b : bs) =
  prettySugarBranch b
    <> mconcat [line <> pipe <+> prettySugarBranch b' | b' <- bs]

-- | Pretty print a sugared branch | --
prettySugarBranch :: (Pattern, SugarCommand) -> Doc ann
prettySugarBranch (pat, cmd) =
  prettyPattern pat
    <+> pretty "->"
    <> line
    <> indent 2 (prettySugarCommand cmd)

-- | Pretty print do-then binding | --
prettyDoThenBinding :: DoThenBinding -> Doc ann
prettyDoThenBinding (Binding pat expr) =
  prettyPattern pat
    <+> pretty "<-"
    <+> prettyTopLevelSugarExpr expr

-- | Pretty print a sugared expression | --
prettyTopLevelSugarExpr :: SugarExpr -> Doc ann
prettyTopLevelSugarExpr (SugarCons con args) =
  case args of
    [] -> pretty con
    _ -> pretty con <+> hsep (map prettySugarExpr args)
prettyTopLevelSugarExpr expr =
  prettySugarExpr expr

prettySugarExpr :: SugarExpr -> Doc ann
prettySugarExpr (AppExpr fun conts args) =
  prettySugarExpr fun
    <> prettyContArgs conts
    <> prettyExprArgs args
prettySugarExpr (CoAppExpr cmdId conts args) =
  pretty cmdId
    <> prettyContArgs conts
    <> prettyExprArgs args
prettySugarExpr (HaveExpr bindings expr) =
  pretty "have"
    <> line
    <> indent 2 (vsep (map prettyHaveBinding bindings))
    <> line
    <> pretty "in"
    <+> prettySugarExpr expr
prettySugarExpr (NatLit n) =
  pretty n
prettySugarExpr (TupLit exprs) =
  parens (hsep (punctuate comma (map prettySugarExpr exprs)))
prettySugarExpr listExpr@(ListLit _) =
  prettySugarListExpr listExpr
prettySugarExpr (SugarCons con args) =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map prettySugarExpr args) <> pretty ")"
prettySugarExpr (SugarMu branches) =
  braces (hang (-1) (prettySugarMuBranches branches))
prettySugarExpr (SugarVar var) =
  pretty var
prettySugarExpr (ThisExpr fieldName) =
  pretty "this" <> pretty "." <> pretty fieldName
prettySugarExpr (MethodCall obj methodName args) =
  prettySugarExpr obj
    <> pretty "::"
    <> pretty methodName
    <> (if null args then mempty else parens (hsep (punctuate comma (map prettySugarExpr args))))
prettySugarExpr (SugarDelimExpr cmd) =
  pretty "<" <> line <> indent 2 (prettySugarCommand cmd) <> line <> pretty ">"

-- | Helper function to pretty print continuation arguments | --
prettyContArgs :: [SugarExpr] -> Doc ann
prettyContArgs [] = mempty
prettyContArgs conts =
  braces (hsep (punctuate comma (map prettyTopLevelSugarExpr conts)))

-- | Helper function to pretty print value arguments | --
prettyExprArgs :: [SugarExpr] -> Doc ann
prettyExprArgs [] = mempty
prettyExprArgs args =
  parens (hsep (punctuate comma (map prettyTopLevelSugarExpr args)))

-- | Helper function to pretty print have bindings | --
prettyHaveBinding :: HaveBinding -> Doc ann
prettyHaveBinding (HaveExprBinding var expr) =
  pretty var
    <+> pretty "="
    <+> prettyTopLevelSugarExpr expr
prettyHaveBinding (HaveCommandBinding cmdId cmd) =
  pretty cmdId
    <+> pretty "="
    <+> prettySugarCommand cmd

-- | Helper function to pretty print sugared list expressions | --
prettySugarListExpr :: SugarExpr -> Doc ann
prettySugarListExpr expr =
  case expr of
    ListLit elements ->
      brackets (hsep (punctuate comma (map prettyTopLevelSugarExpr elements)))
    _ -> error "Expected a list literal"

-- | Helper function to pretty print sugared mu branches | --
prettySugarMuBranches :: [(Pattern, SugarCommand)] -> Doc ann
prettySugarMuBranches [] = mempty
prettySugarMuBranches (b : bs) =
  space
    <> prettySugarBranch b
    <> mconcat [line <> pipe <+> prettySugarBranch b' | b' <- bs]
    <> space

-- | Utility Functions | --

-- | Helper functions to convert to String | --
renderPretty :: Doc ann -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions
