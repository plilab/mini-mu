module Sugar
  ( SugarDecl (..),
    SugarCoDecl (..),
    SugarCommand (..),
    SugarBranch (..),
    DoBinding (..),
    SugarExpr (..),
    SugarCoExpr (..),
  )
where

import Syntax

-- Additional AST nodes for syntactic sugar

-- codef add x y k as <=> add = comu[ Ap3 x y k -> ...]
data SugarDecl
  = DefDecl VarId [Either VarId CoVarId] SugarExpr -- def NAME [PARAMS] as EXPR, def defines a expression
  | RegularDecl Decl

-- codef add x y k as <=> ~add = mu[ Ap3 x y k -> ...]
data SugarCoDecl
  = CoDefDecl CoVarId [Either VarId CoVarId] SugarCoExpr -- codef NAME [PARAMS] as COEXPR, codef defines a co-expression
  | RegularCoDecl Decl

data SugarCommand
  = PipeCommand [Either SugarExpr SugarCoExpr] VarId
  | CoPipeCommand [Either SugarExpr SugarCoExpr] CoVarId -- pipe [PARAMS] into DEFINITION
  | CatchCommand (Either VarId CoVarId) (Either SugarExpr SugarCoExpr) -- catch VAR with COVAR
  | BranchesCommand [SugarBranch] [(String, SugarCommand)] -- branches [...] where ...
  | DoCommand [DoBinding] SugarExpr SugarCoExpr -- do [...] RETURN then CONTINUATION
  | RegularCommand Command -- Original commands

data SugarBranch = SugarBranch Pattern String -- pattern -> continuation_name

data DoBinding = DoBinding String String [String] -- var <- function args

data SugarExpr
  = SugarVar String
  | SugarCons String [Either SugarExpr SugarCoExpr]
  | SugarCoMu [(CoPattern, SugarCommand)]
  | RegularExpr Expr

data SugarCoExpr
  = SugarCoVar String
  | SugarCoCons String [Either SugarExpr SugarCoExpr]
  | SugarMu [(Pattern, SugarCommand)]
  | RegularCoExpr CoExpr