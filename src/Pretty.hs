module Pretty
  ( prettyCommand,
    prettyExpr,
    prettyCoExpr,
    prettyPattern,
    prettyCoPattern,
    showCommand,
    showExpr,
    showCoExpr,
    printCommand,
    printExpr,
    printCoExpr,
    showPattern,
    showCoPattern,
    printPattern,
    printCoPattern,
    renderPretty
  ) where

import Syntax
import Prettyprinter as PP
import Prettyprinter.Render.String

-- Main pretty printing functions that convert syntax to Doc
prettyCommand :: Command -> Doc ann
prettyCommand (Command e k) = angles (prettyExpr e <+> pretty "▷" <+> prettyCoExpr k)

prettyExpr :: Expr -> Doc ann
prettyExpr (Var x) = pretty x
prettyExpr (Con c args) = pretty c <+> hsep (map prettyEitherExpr args)
  where
    prettyEitherExpr (Left e) = prettyExpr e
    prettyEitherExpr (Right k) = prettyCoExpr k
prettyExpr (CoMu cases) = 
  pretty "μ̃" <+> brackets (align (vsep (punctuate (space <> pipe) (map prettyCoCase cases))))
  where
    prettyCoCase (pat, cmd) = prettyCoPattern pat <+> pretty "->" <+> prettyCommand cmd

prettyCoExpr :: CoExpr -> Doc ann
prettyCoExpr (CoVar x) = pretty x
prettyCoExpr (CoCon c args) = pretty c <+> hsep (map prettyEitherExpr args)
  where
    prettyEitherExpr (Left e) = prettyExpr e
    prettyEitherExpr (Right k) = prettyCoExpr k
prettyCoExpr (Mu cases) = 
  pretty "μ" <+> brackets (align (vsep (punctuate (space <> pipe) (map prettyCase cases))))
  where
    prettyCase (pat, cmd) = prettyPattern pat <+> pretty "->" <+> prettyCommand cmd

prettyPattern :: Pattern -> Doc ann
prettyPattern (ConPattern con args) = 
  pretty con <+> hsep (map prettyEitherVar args)
  where
    prettyEitherVar (Left v) = pretty v
    prettyEitherVar (Right cv) = pretty cv
prettyPattern (VarPattern x) = pretty x

prettyCoPattern :: CoPattern -> Doc ann
prettyCoPattern (CoConPattern con args) = 
  pretty con <+> hsep (map prettyEitherVar args)
  where
    prettyEitherVar (Left v) = pretty v
    prettyEitherVar (Right cv) = pretty cv
prettyCoPattern (CoVarPattern x) = pretty x

-- Helper functions to convert to String
renderPretty :: Doc ann -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions

showCommand :: Command -> String
showCommand = renderPretty . prettyCommand

showExpr :: Expr -> String
showExpr = renderPretty . prettyExpr

showCoExpr :: CoExpr -> String
showCoExpr = renderPretty . prettyCoExpr

showPattern :: Pattern -> String
showPattern = renderPretty . prettyPattern

showCoPattern :: CoPattern -> String
showCoPattern = renderPretty . prettyCoPattern

printPattern :: Pattern -> IO ()
printPattern = putStrLn . showPattern

printCoPattern :: CoPattern -> IO ()
printCoPattern = putStrLn . showCoPattern

-- These functions can be used to directly print to the console
printCommand :: Command -> IO ()
printCommand = putStrLn . showCommand

printExpr :: Expr -> IO ()
printExpr = putStrLn . showExpr

printCoExpr :: CoExpr -> IO ()
printCoExpr = putStrLn . showCoExpr