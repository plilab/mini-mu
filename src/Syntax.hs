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
    Value (..),
    Addr (..),
    Context,
    Env (..),
    envEmpty,
    envLookup,
    envLookupCommand,
    envInsert,
    envInsertCommand,
    Store (..),
    storeEmpty,
    storeLookup,
    storeLookupCommand,
    storeAlloc,
    storeAllocCommand,
    storeInsertAddr,
    storeInsertAddrCommand,
    storeInsert,
    storeInsertCommand,
    envStoreInsert,
    envStoreInsertCommand,
    envStoreMerge,
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

data Expr -- e
  = Var VarId -- x
  | Cons ConsId [Expr] -- Foo e k
  | Hole
  | IdiomExpr Command
  | Mu [(Pattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  deriving (Show, Eq, Ord)

data Pattern
  = ConsPattern ConsId [Pattern] -- Cons [nested patterns]
  | VarPattern VarId -- Variable binding: x
  | WildcardPattern -- Wildcard: *
  deriving (Show, Eq, Ord)

data AtomPattern
  = AtomConsPattern ConsId AtomPattern -- Cons without exactly 1 argument
  | AtomVarPattern VarId -- Variable binding: x
  | AtomWildcardPattern -- Wildcard: *
  deriving (Show, Eq, Ord)

data Value
  = ConsValue ConsId [Value]
  | HoleValue
  | MuValue Env [(Pattern, Command)]
  deriving (Show, Eq, Ord)

-- let-and-set
-- CESK: Expr Env Store CoValue
-- Us: Expr Env Store CoExpr / Value (no-Env) Store CoValue

-- | Config represents the state of the computation.

-- It can be a command configuration, a value configuration, or an error configuration.
-- CommandConfig represents a command to be executed with the current environment and store.
-- ValueConfig represents a value and its continuation in the store.
-- ErrorConfig represents an error message.

type Context = (Env, Store, Expr) -- env, store, hole context

data Config
  = CommandConfig Env Store Command -- ρ |- q
  | CommandConfigWithCtx Env Store Context Command
  | ValueConfig Store Value Value
  | ValueConfigWithCtx Store Context Value Value
  | ErrorConfig String
  deriving (Eq, Show, Ord)

newtype Addr = Addr Int deriving (Show, Eq, Ord)

addrInc :: Addr -> Addr
addrInc (Addr n) = Addr (n + 1)

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

envInsert :: Env -> VarId -> Addr -> Env
envInsert (Env env cmdEnv) x addr = Env (Map.insert x addr env) cmdEnv

envInsertCommand :: Env -> CommandId -> Addr -> Env
envInsertCommand (Env env cmdEnv) c addr = Env env (Map.insert c addr cmdEnv)

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

storeAlloc :: Store -> (Addr, Store)
storeAlloc (Store nextAddr nextCmdAddr store _) =
  (nextAddr, Store (addrInc nextAddr) nextCmdAddr store Map.empty)

storeAllocCommand :: Store -> (Addr, Store)
storeAllocCommand (Store nextAddr nextCmdAddr store cmdMap) =
  (nextCmdAddr, Store nextAddr (addrInc nextCmdAddr) store cmdMap)

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

envStoreInsert :: Env -> Store -> VarId -> Value -> (Env, Store)
envStoreInsert env store var value = (env', store')
  where
    env' = envInsert env var addr
    (addr, store') = storeInsert store value

envStoreMerge :: Env -> Store -> Env -> Store -> (Env, Store)
envStoreMerge env1 store1 (Env env2 cmdEnv2) (Store _ _ store2 cmdStore2) =
  let -- First merge all regular values from store2 into env1/store1
      mergeValues = Map.foldrWithKey (\addr value acc -> 
        case acc of 
          (env, store) -> 
            -- Find the variable name that points to this address in env2
            case Map.foldrWithKey (\var varAddr found -> 
              if varAddr == addr then Just var else found) Nothing env2 of
              Just varName -> envStoreInsert env store varName value
              Nothing -> (env, store) -- Skip if no variable points to this address
        ) (env1, store1) store2
      
      -- Then merge all commands from cmdStore2
      mergeCommands = Map.foldrWithKey (\addr cmd acc ->
        case acc of
          (env, store) ->
            -- Find the command name that points to this address in cmdEnv2
            case Map.foldrWithKey (\cmdName cmdAddr found ->
              if cmdAddr == addr then Just cmdName else found) Nothing cmdEnv2 of
              Just cmdName -> envStoreInsertCommand env store cmdName cmd
              Nothing -> (env, store) -- Skip if no command points to this address
        ) mergeValues cmdStore2
  in mergeCommands

envStoreInsertCommand :: Env -> Store -> CommandId -> Command -> (Env, Store)
envStoreInsertCommand env store cmdId cmd = (env', store')
  where
    env' = envInsertCommand env cmdId addr
    (addr, store') = storeInsertCommand store cmd
