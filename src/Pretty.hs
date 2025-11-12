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
  )
where

import qualified Data.Map as Map
import Prettyprinter
import Prettyprinter.Render.String
import Syntax

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
prettyConfig (ErrorConfig string) _ =
  pretty "<Message> " <> pretty string

prettyProgram :: Program -> Doc ann
prettyProgram (Program imports decls exports) =
  pretty "Program"
    <+> braces
      ( line
          <> indent 2 (prettyImports imports)
          <> line
          <> indent 2 (prettyDecls decls)
          <> line
          <> indent 2 (prettyMainExpr exports)
          <> line
      )
  where
    prettyImports [] = mempty
    prettyImports (ImportDecl name _ : is) =
      pretty "Import"
        <+> pretty name
        <> line
        <> prettyImports is
    prettyDecls [] = mempty
    prettyDecls (Decl varId expr : ds) =
      pretty varId
        <+> pretty ":="
        <+> prettyExpr expr
        <> line
        <> prettyDecls ds
    prettyMainExpr es = pretty "Exports:" <+> hsep (map pretty es)

prettyTopLevelValue :: Value -> Bool -> Doc ann
prettyTopLevelValue (ConsValue "Z" []) _ = pretty "0"
prettyTopLevelValue (ConsValue "S" [v]) _ =
  pretty (prettyNatValue (ConsValue "S" [v]))
prettyTopLevelValue listVal@(ConsValue "List::" _) showEnv =
  prettyListValue listVal showEnv
prettyTopLevelValue tupVal@(ConsValue "Tuple" _) showEnv =
  prettyTupleValue tupVal showEnv
prettyTopLevelValue (ConsValue con args) showEnv =
  pretty con <+> hsep (map (`prettyValue` showEnv) args)
prettyTopLevelValue mu@(MuValue _ _) showEnv =
  prettyMuValueAux mu showEnv

prettyValue :: Value -> Bool -> Doc ann
prettyValue (ConsValue "Z" []) _ = pretty "0"
prettyValue (ConsValue "S" [v]) _ =
  pretty (prettyNatValue (ConsValue "S" [v]))
prettyValue listVal@(ConsValue "List::" _) showEnv = prettyListValue listVal showEnv
prettyValue tupVal@(ConsValue "Tuple" _) showEnv = prettyTupleValue tupVal showEnv
prettyValue (ConsValue con args) showEnv =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map (`prettyValue` showEnv) args) <> pretty ")"
prettyValue mu@(MuValue _ _) showEnv =
  prettyMuValueAux mu showEnv

-- Helper function to convert Peano numbers to integers
-- safe cause there is no variable names in values
prettyNatValue :: Value -> Integer
prettyNatValue (ConsValue "Z" []) = 0
prettyNatValue (ConsValue "S" [v]) = 1 + prettyNatValue v
prettyNatValue _ = error "Not a Peano number"

-- Helper function to pretty print List:: values as [v1, v2, ..., vn]
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

-- Helper function to pretty print Tuple values as (v1, v2, ..., vn)
prettyTupleValue :: Value -> Bool -> Doc ann
prettyTupleValue (ConsValue "Tuple" args) showEnv =
  parens (hsep (punctuate comma (map (`prettyValue` showEnv) args)))
prettyTupleValue v showEnv = prettyValue v showEnv

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
        <> prettyCase c
        <> mconcat [line <> pipe <> space <> prettyCase c' | c' <- cs]
prettyMuValueAux _ _ = error "Expected a MuValue"

prettyCommand :: Command -> Doc ann
prettyCommand (Command e k) =
  space
    <> prettyTopLevelExpr e
    <+> pretty "."
    <+> prettyTopLevelExpr k
    <> space
prettyCommand (CommandVar x) = pretty x


prettyTopLevelExpr :: Expr -> Doc ann
prettyTopLevelExpr (Var x) = pretty x
prettyTopLevelExpr (Cons "Z" []) = pretty "0"
prettyTopLevelExpr add1@(Cons "S" _) =
  maybe (prettyNatExprFallback add1) pretty (peanoNatExpr add1)
prettyTopLevelExpr li@(Cons "List::" _) =
  prettyListExpr li
prettyTopLevelExpr tup@(Cons "Tuple" _) =
  prettyTupleExpr tup
prettyTopLevelExpr (Cons con args) =
  pretty con <+> hsep (map prettyExpr args)
prettyTopLevelExpr mu@(Mu _) =
  prettyMuExprAux mu

peanoNatExpr :: Expr -> Maybe Integer
peanoNatExpr (Cons "Z" []) = Just 0
peanoNatExpr (Cons "S" [e]) = (+ 1) <$> peanoNatExpr e
peanoNatExpr _ = Nothing

prettyNatExprFallback :: Expr -> Doc ann
prettyNatExprFallback (Cons "Z" []) = pretty "0"
prettyNatExprFallback (Cons "S" n) = pretty "S" <+> prettyNatExprFallback (head n)
prettyNatExprFallback e = prettyExpr e

-- Helper function to pretty print List:: as [e1, e2, ..., en]
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

-- Helper function to pretty print Tuple as (e1, e2, ..., en)
prettyTupleExpr :: Expr -> Doc ann
prettyTupleExpr (Cons "Tuple" args) =
  parens (hsep (punctuate comma (map prettyExpr args)))
prettyTupleExpr e = prettyExpr e

prettyExpr :: Expr -> Doc ann
prettyExpr (Var x) = pretty x
prettyExpr listExpr@(Cons "List::" _) = prettyListExpr listExpr
prettyExpr tupExpr@(Cons "Tuple" _) = prettyTupleExpr tupExpr
prettyExpr (Cons c args) =
  case args of
    [] -> pretty c
    _ -> pretty "(" <> pretty c <+> hsep (map prettyExpr args) <> pretty ")"
prettyExpr (Mu cases) = prettyMuExprAux (Mu cases)

prettyMuExprAux :: Expr -> Doc ann
prettyMuExprAux (Mu cases) =
  braces (hang (-1) (prettyCases cases))
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyCase c
        <> mconcat [line <> pipe <> space <> prettyCase c' | c' <- cs]
prettyMuExprAux _ = error "Expected a Mu expression"

prettyCase :: (Pattern, Command) -> Doc ann
prettyCase (pat, cmd) =
  prettyPattern pat <+> pretty "->" <> line <> indent 2 (prettyCommand cmd)


prettyPattern :: Pattern -> Doc ann
prettyPattern listPat@(ConsPattern "List::" _) = prettyListPattern listPat
prettyPattern tupPat@(ConsPattern "Tuple" _) = prettyTuplePattern tupPat
prettyPattern (ConsPattern con args) =
  case args of
    [] -> pretty con
    _ -> pretty con <+> hsep (map prettyPattern args)
prettyPattern (VarPattern x) = pretty x
prettyPattern WildcardPattern = pretty "_"

-- Helper function to pretty print List:: patterns as [p1, p2, ..., pn]
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

-- Helper function to pretty print Tuple patterns as (p1, p2, ..., pn)
prettyTuplePattern :: Pattern -> Doc ann
prettyTuplePattern (ConsPattern "Tuple" args) =
  parens (hsep (punctuate comma (map prettyPattern args)))
prettyTuplePattern p = prettyPattern p

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

prettyAddr :: Addr -> Doc ann
prettyAddr (Addr n) = pretty "#" <> pretty n

prettyStore :: Store -> Bool -> Doc ann
prettyStore (Store addr cmdAddr valMap cmdMap) True =
  pretty "Store"
    <+> braces
      ( line
          <> indent
            2
            ( vsep
                [ pretty "Next Address:" <+> prettyAddr addr,
                  pretty "Next Command Address:" <+> prettyAddr cmdAddr,
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

-- Helper functions to convert to String

renderPretty :: Doc ann -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions
