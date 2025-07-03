module Sugar
  ( SugarDecl (..),
    SugarCommand (..),
    SugarBranch (..),
    DoBinding (..),
    SugarExpr (..),
  )
where

import Syntax

-- Additional AST nodes for syntactic sugar

-- codef add x y k as <=> add = comu[ Ap3 x y k -> ...]
data SugarDecl
  = DefDecl VarId [VarId] SugarExpr -- def NAME [PARAMS] as EXPR, def defines a expression
  | RegularDecl Decl

newtype SugarCommand
  = RegularCommand Command -- Original commands

data SugarBranch = SugarBranch Pattern String -- pattern -> continuation_name

data DoBinding = DoBinding String String [String] -- var <- function args

data SugarExpr
  = SugarVar String
  | SugarCons String [Either SugarExpr SugarExpr]
  | SugarMu [(Pattern, SugarCommand)]
  | RegularExpr Expr