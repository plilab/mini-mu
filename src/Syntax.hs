module Syntax
  ( VarId,
    ConId,
    CoVarId,
    CoConId,
    CommandId,
    Program(..),
    Decl(..),
    Command (..),
    Pattern (..),
    Expr (..),
    CoPattern (..),
    CoExpr (..),
    Value (..),
    CoValue (..),
    Env (..),
    envEmpty,
    envLookup,
    envCoLookup,
    envInsert,
    envCoInsert
  )
where

import qualified Data.Map as Map

type VarId = String

type ConId = String

type CoVarId = String

type CoConId = String

type CommandId = String

newtype Program = Program [Decl]

data Decl = Decl VarId Expr | CoDecl CoVarId CoExpr

data Command
  = Command Expr CoExpr -- q = < e >> k >
  | CommandVar CommandId
  deriving (Show, Eq, Ord)

data Pattern
  = ConPattern ConId [Either VarId CoVarId] -- p = Foo x k
  | VarPattern VarId -- p = x
  deriving (Show, Eq, Ord)

data Expr -- e
  = Var VarId -- x
  | Con ConId [Either Expr CoExpr] -- Foo e k
  | CoMu [(CoPattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  deriving (Show, Eq, Ord)

data CoPattern
  = CoConPattern CoConId [Either VarId CoVarId] -- p = Foo x k
  | CoVarPattern CoVarId -- p = alpha
  deriving (Show, Eq, Ord)

data CoExpr -- k
  = CoVar CoVarId -- x
  | CoCon CoConId [Either Expr CoExpr] -- Foo e k
  | Mu [(Pattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  deriving (Show, Eq, Ord)

data Value
  = ConValue ConId [Either Value CoValue]
  | CoMuValue Env [(CoPattern, Command)]
  -- | HaltValue
  deriving (Show, Eq, Ord)

data CoValue
  = CoConValue ConId [Either Value CoValue]
  | MuValue Env [(Pattern, Command)]
  deriving (Show, Eq, Ord)

data Env = Env (Map.Map VarId Value) (Map.Map CoVarId CoValue)
  deriving (Show, Eq, Ord)

envEmpty :: Env
envEmpty = Env Map.empty Map.empty

envLookup :: Env -> VarId -> Value
envLookup (Env env _) x = env Map.! x

envCoLookup :: Env -> CoVarId -> CoValue
envCoLookup (Env _ coenv) x = coenv Map.! x

envInsert :: Env -> VarId -> Value -> Env
envInsert (Env env coenv) x v = Env (Map.insert x v env) coenv

envCoInsert :: Env -> CoVarId -> CoValue -> Env
envCoInsert (Env env coenv) x v = Env env (Map.insert x v coenv)

