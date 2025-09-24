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
    <> prettyCommand command
prettyConfig (ValueConfig store value value') _show =
  pretty "<Value Config>"
    <> line
    <> prettyStore store _show
    <> line
    <> pretty "value:"
    <+> prettyTopLevelValue value _show
    <+> pretty ", co-value:"
    <+> prettyTopLevelValue value' _show
prettyConfig (ErrorConfig string) _ =
  pretty "<Message> " <> pretty string

-- prettyTopLevelEitherValue :: Either Value CoValue -> Doc ann
-- prettyTopLevelEitherValue (Left v) = prettyTopLevelValue v
-- prettyTopLevelEitherValue (Right cv) = prettyTopLevelCoValue cv

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
  pretty (peanoValueToInt (ConsValue "S" [v]))
prettyTopLevelValue (ConsValue con args) showEnv =
  pretty con <+> hsep (map (`prettyValue` showEnv) args)
prettyTopLevelValue (IncompleteConsValue con args) showEnv =
  pretty con <+> hsep (map (`prettyIncompleteConsValueAux` showEnv) args)
prettyTopLevelValue mu@(MuValue _ _) showEnv =
  prettyMuValueAux mu showEnv

-- prettyTopLevelCoValue :: CoValue -> Doc ann
-- prettyTopLevelCoValue (CoConsValue con args) =
--   pretty con <+> hsep (map prettyEitherValue args)
-- prettyTopLevelCoValue mu@(MuValue _ _) =
--   prettyMuValueAux mu

-- prettyEitherValue :: Either Value CoValue -> Doc ann
-- prettyEitherValue (Left v) = prettyValue v
-- prettyEitherValue (Right cv) = prettyCoValue cv

prettyValue :: Value -> Bool -> Doc ann
prettyValue (ConsValue "Z" []) _ = pretty "0"
prettyValue (ConsValue "S" [v]) _ =
  pretty (peanoValueToInt (ConsValue "S" [v]))
prettyValue (ConsValue con args) showEnv =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map (`prettyValue` showEnv) args) <> pretty ")"
prettyValue (IncompleteConsValue con args) showEnv =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map (`prettyIncompleteConsValueAux` showEnv) args) <> pretty ")"
prettyValue mu@(MuValue _ _) showEnv =
  prettyMuValueAux mu showEnv

-- Helper function to convert Peano numbers to integers
-- safe cause there is no variable names in values
peanoValueToInt :: Value -> Integer
peanoValueToInt (ConsValue "Z" []) = 0
peanoValueToInt (ConsValue "S" [v]) = 1 + peanoValueToInt v
peanoValueToInt _ = error "Not a Peano number"

-- prettyCoValue :: CoValue -> Doc ann
-- prettyCoValue (CoConsValue con args) =
--   case args of
--     [] -> pretty con
--     _ -> pretty "(" <> pretty con <+> hsep (map prettyEitherValue args) <> pretty ")"
-- prettyCoValue mu@(MuValue _ _) =
--   prettyMuValueAux mu

{- TODO:
   #9 ↦ MuValue {
      vars = { eq ↦ #4, f ↦ #34, for_loop ↦ #3, if ↦ #5,
               inc ↦ #1, list_map ↦ #0, lt ↦ #2, main ↦ #8,  <-- Note indented line wrapping
               map_once ↦ #6, map_twice ↦ #7, x ↦ #36,
               xs ↦ #35, xs' ↦ #37 },
      coVars = { k ↦ #8 },
      expr = μ[ y ->
          < list_map ▷ Ap3 f xs' μ[ ys' ->
                                    < (List:: y ys') ▷ k >] >]
   }
-}

prettyIncompleteConsValueAux :: Either Value HoleValue -> Bool -> Doc ann
prettyIncompleteConsValueAux (Left v) showEnv = prettyValue v showEnv
prettyIncompleteConsValueAux (Right HoleValue) _ = pretty "_"

prettyMuValueAux :: Value -> Bool -> Doc ann
prettyMuValueAux (MuValue env cases) showEnv =
  pretty "μ-Closure"
    <+> braces
      ( line
          <> indent
            2
            ( prettyEnv env showEnv
                <+> pretty "μ"
                <> brackets (hang (-1) (prettyCases cases))
            )
          <> line
      )
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyCase c
        <> mconcat [line <> pipe <> space <> prettyCase c' | c' <- cs]
prettyMuValueAux _ _ = error "Expected a MuValue"

-- prettyCoMuValueAux :: Value -> Doc ann
-- prettyCoMuValueAux (CoMuValue env cases) =
--   prettyEnv env
--     <+> pretty "μ̃"
--     <> brackets (hang (-1) (prettyCases cases))
--   where
--     prettyCases [] = mempty
--     prettyCases (c : cs) =
--       space
--         <> prettyCoCase c
--         <> mconcat [line <> pipe <> space <> prettyCoCase c' | c' <- cs]
-- prettyCoMuValueAux _ = error "Expected a CoMuValue"

prettyCommand :: Command -> Doc ann
prettyCommand (Command e k) =
  space
    <> pretty "< "
    <> prettyTopLevelExpr e
    <+> pretty "▷"
    <+> prettyTopLevelExpr k
    <> pretty " >"
    <> space
prettyCommand (CommandVar x) = pretty x

-- prettyEitherExpr :: Either Expr CoExpr -> Doc ann
-- prettyEitherExpr (Left e) = prettyExpr e
-- prettyEitherExpr (Right k) = prettyCoExpr k
peanoExprToInt :: Expr -> Maybe Integer
peanoExprToInt (Cons "Z" []) = Just 0
peanoExprToInt (Cons "S" [e]) = (+ 1) <$> peanoExprToInt e
peanoExprToInt _ = Nothing

prettyTopLevelExpr :: Expr -> Doc ann
prettyTopLevelExpr (Var x) = pretty x
prettyTopLevelExpr (Cons "Z" []) = pretty "0"
prettyTopLevelExpr add1@(Cons "S" _) =
  maybe (prettyTopLevelPeanoExprFallback add1) pretty (peanoExprToInt add1)
prettyTopLevelExpr (Cons con args) =
  pretty con <+> hsep (map prettyExpr args)
prettyTopLevelExpr (IncompleteCons con args) =
  pretty con <+> hsep (map prettyIncompleteConsExprAux args)
prettyTopLevelExpr idm@(DerefIdiomExpr _) =
  prettyExpr idm
prettyTopLevelExpr idm@(IdiomExpr _) =
  prettyExpr idm
prettyTopLevelExpr mu@(Mu _) =
  prettyMuExprAux mu

prettyTopLevelPeanoExprFallback :: Expr -> Doc ann
prettyTopLevelPeanoExprFallback (Cons "Z" []) = pretty "0"
prettyTopLevelPeanoExprFallback (Cons "S" n) = pretty "S" <+> prettyTopLevelPeanoExprFallback (head n)
prettyTopLevelPeanoExprFallback e = prettyExpr e

prettyExpr :: Expr -> Doc ann
prettyExpr (Var x) = pretty x
prettyExpr (Cons c args) =
  case args of
    [] -> pretty c
    _ -> pretty "(" <> pretty c <+> hsep (map prettyExpr args) <> pretty ")"
prettyExpr (IncompleteCons con args) =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map prettyIncompleteConsExprAux args) <> pretty ")"
prettyExpr (DerefIdiomExpr cmd) = pretty "*[" <> prettyCommand cmd <> pretty "]"
prettyExpr (IdiomExpr cmd) = pretty "[" <> prettyCommand cmd <> pretty "]"
prettyExpr (Mu cases) = prettyMuExprAux (Mu cases)

prettyIncompleteConsExprAux :: Either Expr HoleExpr -> Doc ann
prettyIncompleteConsExprAux (Left e) = prettyExpr e
prettyIncompleteConsExprAux (Right HoleExpr) = pretty "_"

prettyMuExprAux :: Expr -> Doc ann
prettyMuExprAux (Mu cases) =
  pretty "μ" <> brackets (hang (-1) (prettyCases cases))
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyCase c
        <> mconcat [line <> pipe <> space <> prettyCase c' | c' <- cs]
prettyMuExprAux _ = error "Expected a Mu expression"

-- prettyCoExpr :: CoExpr -> Doc ann
-- prettyCoExpr (CoVar x) = pretty x
-- prettyCoExpr (CoCons c args) =
--   pretty c <+> hsep (map prettyEitherExpr args)
-- prettyCoExpr (Mu cases) =
--   pretty "μ"
--     <> brackets (hang (-1) (prettyCases cases))
--   where
--     prettyCases [] = mempty
--     prettyCases (c : cs) =
--       space
--         <> prettyCase c
--         <> mconcat [line <> pipe <> space <> prettyCase c' | c' <- cs]

prettyCase :: (Pattern, Command) -> Doc ann
prettyCase (pat, cmd) =
  prettyPattern pat <+> pretty "->" <> line <> indent 2 (prettyCommand cmd)

-- prettyCoCase :: (CoPattern, Command) -> Doc ann
-- prettyCoCase (pat, cmd) =
--   prettyCoPattern pat <+> pretty "->" <> line <> indent 2 (prettyCommand cmd)

prettyPattern :: Pattern -> Doc ann
prettyPattern (ConsPattern con args) =
  pretty con <+> hsep (map prettyPattern args)
prettyPattern (VarPattern x) = pretty x
prettyPattern WildcardPattern = pretty "_"

-- prettyCoPattern :: CoPattern -> Doc ann
-- prettyCoPattern (CoConsPattern con args) =
--   pretty con <+> hsep (map prettyEitherVar args)
-- prettyCoPattern (CoVarPattern x) = pretty x

-- prettyEitherVar :: Either VarId CoVarId -> Doc ann
-- prettyEitherVar (Left v) = pretty v
-- prettyEitherVar (Right cv) = pretty cv

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

-- prettyCoAddr :: CoAddr -> Doc ann
-- prettyCoAddr (CoAddr n) = pretty "#" <> pretty n

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
