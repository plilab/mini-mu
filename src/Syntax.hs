module Syntax
  ( VarId,
    ConId,
    CoVarId,
    CoConId,
    Command (..),
    Pattern (..),
    Expr (..),
    CoPattern (..),
    CoExpr (..),
    Value (..),
    CoValue (..),
    Env (..),
    envLookup,
  )
where

import qualified Data.Map as Map

type VarId = String

type ConId = String

type CoVarId = String

type CoConId = String

data Command = Command Expr CoExpr -- q = < e | k >

data Pattern
  = ConPattern ConId [Either VarId CoVarId] -- p = Foo x k
  | VarPattern VarId -- p = x

data Expr -- e
  = Var VarId -- x
  | Con ConId [Either Expr CoExpr] -- Foo e k
  | CoMu [(CoPattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  -- set!, print

data CoPattern
  = CoConPattern CoConId [Either VarId CoVarId] -- p = Foo x k
  | CoVarPattern CoVarId -- p = alpha

data CoExpr -- k
  = CoVar CoVarId -- x
  | CoCon CoConId [Either Expr CoExpr] -- Foo e k
  | Mu [(Pattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]

data Value
  = ConValue ConId [Either Value CoValue]
  | CoMuValue Env [(CoPattern, Command)]

data CoValue
  = CoConValue ConId [Either Value CoValue]
  | MuValue Env [(Pattern, Command)]

data Env = Env (Map.Map VarId Value) (Map.Map CoVarId CoValue)

envLookup :: Env -> VarId -> Value
envLookup (Env env _) x = env Map.! x