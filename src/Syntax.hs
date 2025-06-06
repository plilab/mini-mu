module Syntax
  ( VarId,
    ConsId,
    CoVarId,
    CoConsId,
    CommandId,
    Program (..),
    Decl (..),
    Command (..),
    Pattern (..),
    Expr (..),
    CoPattern (..),
    CoExpr (..),
    Value (..),
    CoValue (..),
    Addr (..),
    CoAddr (..),
    Env (..),
    envEmpty,
    envLookup,
    envCoLookup,
    envInsert,
    envCoInsert,
    Store (..),
    storeEmpty,
    storeLookup,
    storeCoLookup,
    storeAlloc,
    storeCoAlloc,
    storeInsertAddr,
    storeInsert,
    storeCoInsert,
    envStoreInsert,
    envStoreCoInsert,
    Config (..),
  )
where

import qualified Data.Map as Map

type VarId = String

type ConsId = String

type CoVarId = String

type CoConsId = String

type CommandId = String

-- let-and-set
-- CESK: Expr Env Store CoValue
-- Us: Expr Env Store CoExpr / Value (no-Env) Store CoValue
data Config
  = CommandConfig Env Store Command -- ρ |- q
  | ValueConfig Store Value CoValue
  | ErrorConfig String
  deriving (Eq, Show, Ord)

newtype Program = Program [Decl]

data Decl = Decl VarId Expr | CoDecl CoVarId CoExpr

data Command
  = Command Expr CoExpr -- q = < e >> k >
  | CommandVar CommandId
  deriving (Show, Eq, Ord)

data Pattern
  = ConsPattern ConsId [Either VarId CoVarId] -- p = Foo x k
  | VarPattern VarId -- p = x
  deriving (Show, Eq, Ord)

data Expr -- e
  = Var VarId -- x
  | Cons ConsId [Either Expr CoExpr] -- Foo e k
  | CoMu [(CoPattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  deriving (Show, Eq, Ord)

data CoPattern
  = CoConsPattern CoConsId [Either VarId CoVarId] -- p = Foo x k
  | CoVarPattern CoVarId -- p = alpha
  deriving (Show, Eq, Ord)

data CoExpr -- k
  = CoVar CoVarId -- x
  | CoCons CoConsId [Either Expr CoExpr] -- Foo e k
  | Mu [(Pattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  deriving (Show, Eq, Ord)

data Value
  = ConsValue ConsId [Either Value CoValue]
  | CoMuValue Env [(CoPattern, Command)]
  deriving
    ( -- | HaltValue
      Show,
      Eq,
      Ord
    )

data CoValue
  = CoConsValue ConsId [Either Value CoValue]
  | MuValue Env [(Pattern, Command)]
  deriving (Show, Eq, Ord)

newtype Addr = Addr Int deriving (Show, Eq, Ord)

addrInc :: Addr -> Addr
addrInc (Addr n) = Addr (n + 1)

newtype CoAddr = CoAddr Int deriving (Show, Eq, Ord)

coAddrInc :: CoAddr -> CoAddr
coAddrInc (CoAddr n) = CoAddr (n + 1)

data Env = Env (Map.Map VarId Addr) (Map.Map CoVarId CoAddr)
  deriving (Show, Eq, Ord)

envEmpty :: Env
envEmpty = Env Map.empty Map.empty

envLookup :: Env -> VarId -> Addr
envLookup (Env env _) x = env Map.! x

envCoLookup :: Env -> CoVarId -> CoAddr
envCoLookup (Env _ coenv) x = coenv Map.! x

envInsert :: Env -> VarId -> Addr -> Env
envInsert (Env env coenv) x v = Env (Map.insert x v env) coenv

envCoInsert :: Env -> CoVarId -> CoAddr -> Env
envCoInsert (Env env coenv) x v = Env env (Map.insert x v coenv)

data Store = Store Addr CoAddr (Map.Map Addr Value) (Map.Map CoAddr CoValue)
  deriving (Show, Eq, Ord)

storeEmpty :: Store
storeEmpty = Store (Addr 0) (CoAddr 0) Map.empty Map.empty

storeLookup :: Store -> Addr -> Value
storeLookup (Store _ _ store _) x = store Map.! x

storeCoLookup :: Store -> CoAddr -> CoValue
storeCoLookup (Store _ _ _ costore) x = costore Map.! x

storeAlloc :: Store -> (Addr, Store)
storeAlloc (Store nextAddr nextCoAddr store costore) =
  (nextAddr, Store (addrInc nextAddr) nextCoAddr store costore)

storeCoAlloc :: Store -> (CoAddr, Store)
storeCoAlloc (Store nextAddr nextCoAddr store costore) =
  (nextCoAddr, Store nextAddr (coAddrInc nextCoAddr) store costore)

storeInsertAddr :: Store -> Addr -> Value -> Store
storeInsertAddr (Store nextAddr nextCoAddr store costore) addr v =
  Store nextAddr nextCoAddr (Map.insert addr v store) costore

storeInsert :: Store -> Value -> (Addr, Store) -- TODO: use storeInsertAddr
storeInsert (Store nextAddr nextCoAddr store costore) v =
  (nextAddr, Store (addrInc nextAddr) nextCoAddr (Map.insert nextAddr v store) costore)

storeCoInsert :: Store -> CoValue -> (CoAddr, Store)
storeCoInsert (Store nextAddr nextCoAddr store costore) v =
  (nextCoAddr, Store nextAddr (coAddrInc nextCoAddr) store (Map.insert nextCoAddr v costore))

envStoreInsert :: Env -> Store -> VarId -> Value -> (Env, Store)
envStoreInsert env store var value = (env', store')
  where
    env' = envInsert env var addr
    (addr, store') = storeInsert store value

envStoreCoInsert :: Env -> Store -> CoVarId -> CoValue -> (Env, Store)
envStoreCoInsert env store var value = (env', store')
  where
    env' = envCoInsert env var coAddr
    (coAddr, store') = storeCoInsert store value
