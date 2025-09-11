module Syntax
  ( VarId,
    ConsId,
    CommandId,
    ImportDecl (..),
    Program (..),
    Decl (..),
    Command (..),
    Pattern (..),
    Expr (..),
    HoleExpr (..),
    -- CoPattern (..),
    -- CoExpr (..),
    Value (..),
    HoleValue (..),
    -- CoValue (..),
    Addr (..),
    -- CoAddr (..),
    Env (..),
    envEmpty,
    envLookup,
    envLookupCommand,
    -- envCoLookup,
    envInsert,
    envInsertCommand,
    -- envCoInsert,
    Store (..),
    storeEmpty,
    storeLookup,
    storeLookupCommand,
    -- storeCoLookup,
    storeAlloc,
    storeAllocCommand,
    -- storeCoAlloc,
    storeInsertAddr,
    storeInsertAddrCommand,
    storeInsert,
    storeInsertCommand,
    -- storeCoInsert,
    envStoreInsert,
    envStoreInsertCommand,
    -- envStoreCoInsert,
    Config (..),
  )
where

import qualified Data.Map as Map

type VarId = String

type ConsId = String

-- type CoVarId = String
-- type CoConsId = String

type CommandId = String

data Program = Program
  { programImports :: [ImportDecl], -- at beginning
    programDecls :: [Decl], -- middle
    programExports :: [VarId] -- at end
  }
  deriving (Show, Eq, Ord)

data ImportDecl = ImportDecl String [VarId] -- import "module" (x, y, z)
  deriving (Show, Eq, Ord)

data Decl
  = Decl VarId Expr -- a = e
  deriving (Show, Eq, Ord)

data Command
  = Command Expr Expr -- q = < e |> k >
  | CommandVar CommandId
  deriving (Show, Eq, Ord)

data HoleExpr = HoleExpr deriving (Show, Eq, Ord)

data Expr -- e
  = Var VarId -- x
  | Cons ConsId [Expr] -- Foo e k
  | IncompleteCons ConsId [Either Expr HoleExpr] -- Foo e _ e
  | Mu [(Pattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  deriving (Show, Eq, Ord)

data Pattern
  = ConsPattern ConsId [Pattern] -- Cons [nested patterns]
  | VarPattern VarId -- Variable binding: x
  | WildcardPattern -- Wildcard: *
  deriving (Show, Eq, Ord)

-- data CoPattern
--   = CoConsPattern CoConsId [Either VarId CoVarId] -- p = Foo x k
--   | CoVarPattern CoVarId -- p = alpha
--   deriving (Show, Eq, Ord)

-- data CoExpr -- k
--   = CoVar CoVarId -- x
--   | CoCons CoConsId [Either Expr CoExpr] -- Foo e k
--   | Mu [(Pattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
--   deriving (Show, Eq, Ord)

data HoleValue = HoleValue deriving (Show, Eq, Ord)

data Value
  = ConsValue ConsId [Value]
  | IncompleteConsValue ConsId [Either Value HoleValue]
  | MuValue Env [(Pattern, Command)]
  deriving (Show, Eq, Ord)

-- data CoValue
--   = CoConsValue ConsId [Either Value CoValue]
--   | MuValue Env [(Pattern, Command)]
--   deriving (Show, Eq, Ord)

-- let-and-set
-- CESK: Expr Env Store CoValue
-- Us: Expr Env Store CoExpr / Value (no-Env) Store CoValue

-- | Config represents the state of the computation.
-- It can be a command configuration, a value configuration, or an error configuration.
-- CommandConfig represents a command to be executed with the current environment and store.
-- ValueConfig represents a value and its continuation in the store.
-- ErrorConfig represents an error message.
data Config
  = CommandConfig Env Store Command -- ρ |- q
  | ValueConfig Store Value Value
  | ErrorConfig String
  deriving (Eq, Show, Ord)

newtype Addr = Addr Int deriving (Show, Eq, Ord)

addrInc :: Addr -> Addr
addrInc (Addr n) = Addr (n + 1)

-- newtype CoAddr = CoAddr Int deriving (Show, Eq, Ord)

-- coAddrInc :: CoAddr -> CoAddr
-- coAddrInc (CoAddr n) = CoAddr (n + 1)

-- | Env is a mapping from variable identifiers to addresses.
data Env = Env (Map.Map VarId Addr) (Map.Map CommandId Addr)
  deriving (Show, Eq, Ord)

envEmpty :: Env
envEmpty = Env Map.empty Map.empty

envLookup :: Env -> VarId -> Addr
envLookup (Env env _) x = case Map.lookup x env of
  Just addr -> addr
  Nothing -> error $ "Variable not found in environment: " ++ x

envLookupCommand :: Env -> CommandId -> Addr
envLookupCommand (Env _ cmdEnv) x = case Map.lookup x cmdEnv of
  Just addr -> addr
  Nothing -> error $ "Command not found in environment: " ++ x

-- envCoLookup :: Env -> CoVarId -> CoAddr
-- envCoLookup (Env _ coenv) x = coenv Map.! x

envInsert :: Env -> VarId -> Addr -> Env
envInsert (Env env cmdEnv) x addr = Env (Map.insert x addr env) cmdEnv

envInsertCommand :: Env -> CommandId -> Addr -> Env
envInsertCommand (Env env cmdEnv) c addr = Env env (Map.insert c addr cmdEnv)

-- envCoInsert :: Env -> CoVarId -> CoAddr -> Env
-- envCoInsert (Env env coenv) x v = Env env (Map.insert x v coenv)

-- data Store = Store Addr CoAddr (Map.Map Addr Value) (Map.Map CoAddr CoValue)
--   deriving (Show, Eq, Ord)

-- | Definition of Store, which maps addresses to values.
-- It also contains the next available address for allocation.
data Store = Store Addr Addr (Map.Map Addr Value) (Map.Map Addr Command)
  deriving (Show, Eq, Ord)

storeEmpty :: Store
storeEmpty = Store (Addr 0) (Addr 0) Map.empty Map.empty

storeLookup :: Store -> Addr -> Value
storeLookup (Store _ _ store _) x = case Map.lookup x store of
  Just val -> val
  Nothing -> error $ "Value not found in store: " ++ show x

storeLookupCommand :: Store -> Addr -> Command
storeLookupCommand (Store _ _ _ cmdMap) x = case Map.lookup x cmdMap of
  Just cmd -> cmd
  Nothing -> error $ "Command not found in store: " ++ show x

-- storeCoLookup :: Store -> CoAddr -> CoValue
-- storeCoLookup (Store _ _ _ costore) x = costore Map.! x

storeAlloc :: Store -> (Addr, Store)
storeAlloc (Store nextAddr nextCmdAddr store _) =
  (nextAddr, Store (addrInc nextAddr) nextCmdAddr store Map.empty)

storeAllocCommand :: Store -> (Addr, Store)
storeAllocCommand (Store nextAddr nextCmdAddr store cmdMap) =
  (nextCmdAddr, Store nextAddr (addrInc nextCmdAddr) store cmdMap)

-- storeCoAlloc :: Store -> (CoAddr, Store)
-- storeCoAlloc (Store nextAddr nextCoAddr store costore) =
--   (nextCoAddr, Store nextAddr (coAddrInc nextCoAddr) store costore)

-- | These functions insert a value or command into the store at a specific address.
storeInsertAddr :: Store -> Addr -> Value -> Store
storeInsertAddr (Store nextAddr nextCmdAddr store cmdStore) addr v =
  Store nextAddr nextCmdAddr (Map.insert addr v store) cmdStore

storeInsertAddrCommand :: Store -> Addr -> Command -> Store
storeInsertAddrCommand (Store nextAddr nextCmdAddr store cmdStore) addr cmd =
  Store nextAddr nextCmdAddr store (Map.insert addr cmd cmdStore)

-- | These functions insert a NEW value or command into the store and return the address where it was inserted.
storeInsert :: Store -> Value -> (Addr, Store) -- TODO: use storeInsertAddr
storeInsert (Store nextAddr nextCmdAddr store cmdStore) v =
  (nextAddr, Store (addrInc nextAddr) nextCmdAddr (Map.insert nextAddr v store) cmdStore)

storeInsertCommand :: Store -> Command -> (Addr, Store)
storeInsertCommand (Store nextAddr nextCmdAddr store cmdStore) cmd =
  (nextCmdAddr, Store nextAddr (addrInc nextCmdAddr) store (Map.insert nextCmdAddr cmd cmdStore))

-- storeCoInsert :: Store -> CoValue -> (CoAddr, Store)
-- storeCoInsert (Store nextAddr nextCoAddr store costore) v =
--   (nextCoAddr, Store nextAddr (coAddrInc nextCoAddr) store (Map.insert nextCoAddr v costore))

envStoreInsert :: Env -> Store -> VarId -> Value -> (Env, Store)
envStoreInsert env store var value = (env', store')
  where
    env' = envInsert env var addr
    (addr, store') = storeInsert store value

envStoreInsertCommand :: Env -> Store -> CommandId -> Command -> (Env, Store)
envStoreInsertCommand env store cmdId cmd = (env', store')
  where
    env' = envInsertCommand env cmdId addr
    (addr, store') = storeInsertCommand store cmd

-- envStoreCoInsert :: Env -> Store -> CoVarId -> CoValue -> (Env, Store)
-- envStoreCoInsert env store var value = (env', store')
--   where
--     env' = envCoInsert env var coAddr
--     (coAddr, store') = storeCoInsert store value
