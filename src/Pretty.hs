module Pretty
  ( prettyConfig,
    prettyCommand,
    prettyExpr,
    prettyCoExpr,
    prettyPattern,
    prettyCoPattern,
    renderPretty,
    prettyTopLevelEitherValue,
    prettyTopLevelValue,
    prettyTopLevelCoValue,
  )
where

import qualified Data.Map as Map
import Prettyprinter
import Prettyprinter.Render.String
import Syntax

prettyConfig :: Config -> Doc ann
prettyConfig (CommandConfig env store command) =
  pretty "<Command Config> "
    <> prettyEnv env
    <> prettyStore store
    <> prettyCommand command
prettyConfig (ValueConfig store value coValue) =
  pretty "<Value Config> "
    <> prettyStore store
    <> prettyValue value
    <> prettyCoValue coValue
prettyConfig (ErrorConfig string) =
  pretty "<Message> " <> pretty string

prettyTopLevelEitherValue :: Either Value CoValue -> Doc ann
prettyTopLevelEitherValue (Left v) = prettyTopLevelValue v
prettyTopLevelEitherValue (Right cv) = prettyTopLevelCoValue cv

prettyTopLevelValue :: Value -> Doc ann
prettyTopLevelValue (ConsValue con args) =
  pretty con <+> hsep (map prettyEitherValue args)
prettyTopLevelValue comu@(CoMuValue _ _) =
  prettyCoMuValueAux comu

prettyTopLevelCoValue :: CoValue -> Doc ann
prettyTopLevelCoValue (CoConsValue con args) =
  pretty con <+> hsep (map prettyEitherValue args)
prettyTopLevelCoValue mu@(MuValue _ _) =
  prettyMuValueAux mu

prettyValue :: Value -> Doc ann
prettyValue (ConsValue con args) =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map prettyEitherValue args) <> pretty ")"
prettyValue comu@(CoMuValue _ _) =
  prettyCoMuValueAux comu

prettyCoValue :: CoValue -> Doc ann
prettyCoValue (CoConsValue con args) =
  case args of
    [] -> pretty con
    _ -> pretty "(" <> pretty con <+> hsep (map prettyEitherValue args) <> pretty ")"
prettyCoValue mu@(MuValue _ _) =
  prettyMuValueAux mu

prettyMuValueAux :: CoValue -> Doc ann
prettyMuValueAux (MuValue env cases) =
  prettyEnv env
    <+> pretty "μ"
    <> brackets (hang (-1) (prettyCases cases))
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyCase c
        <> mconcat [line <> pipe <> space <> prettyCase c' | c' <- cs]
prettyMuValueAux _ = error "Expected a MuValue"

prettyCoMuValueAux :: Value -> Doc ann
prettyCoMuValueAux (CoMuValue env cases) =
  prettyEnv env
    <+> pretty "μ̃"
    <> brackets (hang (-1) (prettyCases cases))
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyCoCase c
        <> mconcat [line <> pipe <> space <> prettyCoCase c' | c' <- cs]
prettyCoMuValueAux _ = error "Expected a CoMuValue"

prettyEitherValue :: Either Value CoValue -> Doc ann
prettyEitherValue (Left v) = prettyValue v
prettyEitherValue (Right cv) = prettyCoValue cv

prettyCommand :: Command -> Doc ann
prettyCommand (Command e k) =
  pretty "< " <> prettyExpr e <+> pretty "▷" <+> prettyCoExpr k <> pretty " >"
prettyCommand (CommandVar x) = pretty x

prettyExpr :: Expr -> Doc ann
prettyExpr (Var x) = pretty x
prettyExpr (Cons c args) =
  case args of
    [] -> pretty c
    _ -> pretty "(" <> pretty c <+> hsep (map prettyEitherExpr args) <> pretty ")"
prettyExpr (CoMu cases) =
  pretty "μ̃"
    <> brackets (hang (-1) (prettyCases cases))
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyCoCase c
        <> mconcat [line <> pipe <> space <> prettyCoCase c' | c' <- cs]

prettyCoExpr :: CoExpr -> Doc ann
prettyCoExpr (CoVar x) = pretty x
prettyCoExpr (CoCons c args) =
  pretty c <+> hsep (map prettyEitherExpr args)
prettyCoExpr (Mu cases) =
  pretty "μ"
    <> brackets (hang (-1) (prettyCases cases))
  where
    prettyCases [] = mempty
    prettyCases (c : cs) =
      space
        <> prettyCase c
        <> mconcat [line <> pipe <> space <> prettyCase c' | c' <- cs]

prettyEitherExpr :: Either Expr CoExpr -> Doc ann
prettyEitherExpr (Left e) = prettyExpr e
prettyEitherExpr (Right k) = prettyCoExpr k

prettyCase :: (Pattern, Command) -> Doc ann
prettyCase (pat, cmd) =
  prettyPattern pat <+> pretty "->" <> line <> indent 2 (prettyCommand cmd)

prettyCoCase :: (CoPattern, Command) -> Doc ann
prettyCoCase (pat, cmd) =
  prettyCoPattern pat <+> pretty "->" <> line <> indent 2 (prettyCommand cmd)

prettyPattern :: Pattern -> Doc ann
prettyPattern (ConsPattern con args) =
  pretty con <+> hsep (map prettyEitherVar args)
prettyPattern (VarPattern x) = pretty x

prettyCoPattern :: CoPattern -> Doc ann
prettyCoPattern (CoConsPattern con args) =
  pretty con <+> hsep (map prettyEitherVar args)
prettyCoPattern (CoVarPattern x) = pretty x

prettyEitherVar :: Either VarId CoVarId -> Doc ann
prettyEitherVar (Left v) = pretty v
prettyEitherVar (Right cv) = pretty cv

prettyEnv :: Env -> Doc ann
prettyEnv env =
  pretty "Env"
    <+> braces
      ( line
          <> indent
            2
            ( vsep
                [ pretty "vars:" <+> prettyVarMap (envVars env),
                  pretty "co-vars:" <+> prettyCoVarMap (envCoVars env)
                ]
            )
          <> line
      ) <> line
  where
    envVars :: Env -> Map.Map VarId Addr
    envVars (Env m _) = m
    envCoVars :: Env -> Map.Map CoVarId CoAddr
    envCoVars (Env _ m) = m
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
    prettyCoVarMap :: Map.Map CoVarId CoAddr -> Doc ann
    prettyCoVarMap m
      | Map.null m = pretty "{}"
      | otherwise =
          braces
            ( line
                <> indent
                  2
                  ( vsep
                      [ pretty cv
                          <+> pretty "↦"
                          <+> prettyCoAddr coAddr
                        | (cv, coAddr) <- Map.toList m
                      ]
                  )
                <> line
            )

prettyAddr :: Addr -> Doc ann
prettyAddr (Addr n) = pretty "#" <> pretty n

prettyCoAddr :: CoAddr -> Doc ann
prettyCoAddr (CoAddr n) = pretty "#" <> pretty n

prettyStore :: Store -> Doc ann
prettyStore (Store addr coAddr valMap coValMap) =
  pretty "Store"
    <+> braces
      ( line
          <> indent
            2
            ( vsep
                [ pretty "current:" <+> prettyAddr addr,
                  pretty "co-current:" <+> prettyCoAddr coAddr,
                  pretty "values:" <+> prettyValueMap valMap,
                  pretty "co-values:" <+> prettyCoValueMap coValMap
                ]
            )
          <> line
      ) <> line
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
                      [ prettyAddr addr1
                          <+> pretty "↦"
                          <+> prettyTopLevelValue val
                        | (addr1, val) <- Map.toList m
                      ]
                  )
                <> line
            )
    prettyCoValueMap :: Map.Map CoAddr CoValue -> Doc ann
    prettyCoValueMap m
      | Map.null m = pretty "{}"
      | otherwise =
          braces
            ( line
                <> indent
                  2
                  ( vsep
                      [ prettyCoAddr coAddr1
                          <+> pretty "↦"
                          <+> prettyTopLevelCoValue coVal
                        | (coAddr1, coVal) <- Map.toList m
                      ]
                  )
                <> line
            )

-- Helper functions to convert to String

renderPretty :: Doc ann -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions
