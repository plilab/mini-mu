module Syntax
  ( VarId,
    ConsId,
    CommandId,
    ImportDecl (..),
    Program (..),
    SugarDecl (..),
    DoThenBinding (..),
    SugarCommand (..),
    HaveBinding (..),
    SugarExpr (..),
    Decl (..),
    Command (..),
    Pattern (..),
    Expr (..),
    Value (..),
    Addr (..),
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

-- | Sugared Syntax | --

-- | Sugared Declarations | --
data SugarDecl
  = FuncDecl VarId [VarId] SugarExpr -- fn f x y z = e
  | DefaultDecl VarId SugarExpr
  deriving (Show, Eq, Ord)

-- | Bindings for do/then syntax | --
data DoThenBinding = Binding VarId SugarExpr -- x <- e
  deriving (Show, Eq, Ord)

-- | Sugared Commands | --
data SugarCommand
  = LetCommand VarId SugarExpr SugarCommand -- let x = e in q
  | LetcCommand VarId SugarExpr SugarCommand -- letc x = e in q
  | MatchCommand SugarExpr [(Pattern, SugarCommand)] -- match e with p1 -> q1 | p2 -> q2 | ...
  | PatchCommand SugarExpr [(Pattern, SugarCommand)] -- patch e with p1 -> q1 | p2 -> q2 | ...
  | DoThenCommand [DoThenBinding] SugarCommand -- do binding* then q
  | AtCommand SugarExpr [SugarExpr] -- f @ a b c
  | DotCommand SugarExpr SugarExpr -- p . c
  | SugarCommandVar CommandId
  deriving (Show, Eq, Ord)

-- | Bindings for have syntax | --
data HaveBinding 
  = HaveExprBinding VarId SugarExpr -- have x = e
  | HaveCommandBinding CommandId SugarCommand -- have 'c = e
  deriving (Show, Eq, Ord)

-- | Sugared Expressions | --
data SugarExpr
  = AppExpr SugarExpr [SugarExpr] [SugarExpr] -- f {k1, k2} (x1, x2, k1, k2)
  | CoAppExpr CommandId [SugarExpr] [SugarExpr] -- 'f {k1, k2} (x1, x2, k1, k2)
  | HaveExpr [HaveBinding] SugarExpr -- have bindings* in e
  | DelimExpr SugarCommand -- < ... >
  | NatLit Integer -- 42
  | TupLit [SugarExpr] -- (e1, e2, ..., en)
  | SugarCons ConsId [SugarExpr] -- Foo e1 e2 ... en
  | SugarMu [(Pattern, SugarCommand)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  | SugarVar VarId -- x
  deriving (Show, Eq, Ord)

-- | Core Syntax | --

-- | Core Declarations | --
data Decl
  = Decl VarId Expr -- a = e
  deriving (Show, Eq, Ord)

-- | Core Commands | --
data Command
  = Command Expr Expr -- e . k
  | CommandVar CommandId
  deriving (Show, Eq, Ord)

-- | Core Expressions | --
data Expr -- e
  = Var VarId -- x
  | Cons ConsId [Expr] -- Foo e k
  | Mu [(Pattern, Command)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  deriving (Show, Eq, Ord)

-- | Patterns | --
data Pattern
  = ConsPattern ConsId [Pattern] -- Cons [nested patterns]
  | VarPattern VarId -- Variable binding: x
  | WildcardPattern -- Wildcard: *
  deriving (Show, Eq, Ord)

-- | Values | --
data Value
  = ConsValue ConsId [Value]
  | MuValue Env [(Pattern, Command)]
  deriving (Show, Eq, Ord)

-- | Abstract Machine Setups | --

-- let-and-set
-- CESK: Expr Env Store CoValue
-- Us: Expr Env Store CoExpr / Value (no-Env) Store CoValue

-- | Config represents the state of the computation | --
data Config
  = CommandConfig Env Store Command -- command to be executed with the current environment and store.
  | ValueConfig Store Value Value -- value and its continuation in the store
  | ErrorConfig String -- error message
  deriving (Eq, Show, Ord)

-- | Definition of Addr, Env, and Store | --

-- | Addr represents an address in the store | --
newtype Addr = Addr Int deriving (Show, Eq, Ord)

-- | Function to increment an address | --
addrInc :: Addr -> Addr
addrInc (Addr n) = Addr (n + 1)

-- | Env is a mapping from variable identifiers to addresses | --
data Env = Env (Map.Map VarId Addr) (Map.Map CommandId Addr)
  deriving (Show, Eq, Ord)

-- | Functions to manipulate the environment  --

-- | Create an empty environment | --
envEmpty :: Env
envEmpty = Env Map.empty Map.empty

-- | Lookup functions for variables and commands in the environment | --
envLookup :: Env -> VarId -> Addr
envLookup (Env env _) x = case Map.lookup x env of
  Just addr -> addr
  Nothing -> error $ "Variable not found in environment: " ++ x

-- | Lookup command address in the environment | --
envLookupCommand :: Env -> CommandId -> Addr
envLookupCommand (Env _ cmdEnv) x = case Map.lookup x cmdEnv of
  Just addr -> addr
  Nothing -> error $ "Command not found in environment: " ++ x

-- | Insert functions for variables and commands in the environment | --
envInsert :: Env -> VarId -> Addr -> Env
envInsert (Env env cmdEnv) x addr = Env (Map.insert x addr env) cmdEnv

-- | Insert command into the environment | --
envInsertCommand :: Env -> CommandId -> Addr -> Env
envInsertCommand (Env env cmdEnv) c addr = Env env (Map.insert c addr cmdEnv)

-- | Definition of Store, which maps addresses to values | --
data Store = Store Addr Addr (Map.Map Addr Value) (Map.Map Addr Command)
  deriving (Show, Eq, Ord)

-- | Functions to manipulate the store | --
storeEmpty :: Store
storeEmpty = Store (Addr 0) (Addr 0) Map.empty Map.empty

-- | These functions lookup a value or command in the store by its address | --
storeLookup :: Store -> Addr -> Value
storeLookup (Store _ _ store _) x = case Map.lookup x store of
  Just val -> val
  Nothing -> error $ "Value not found in store: " ++ show x

-- | Lookup command in the store | --
storeLookupCommand :: Store -> Addr -> Command
storeLookupCommand (Store _ _ _ cmdMap) x = case Map.lookup x cmdMap of
  Just cmd -> cmd
  Nothing -> error $ "Command not found in store: " ++ show x

-- | Allocate a new address for a value or command in the store | --
storeAlloc :: Store -> (Addr, Store)
storeAlloc (Store nextAddr nextCmdAddr store _) =
  (nextAddr, Store (addrInc nextAddr) nextCmdAddr store Map.empty)

-- | Allocate a new address for a command in the store | --
storeAllocCommand :: Store -> (Addr, Store)
storeAllocCommand (Store nextAddr nextCmdAddr store cmdMap) =
  (nextCmdAddr, Store nextAddr (addrInc nextCmdAddr) store cmdMap)

-- | Insert a value or command into the store at a specific address | --
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
