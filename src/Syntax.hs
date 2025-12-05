module Syntax
  ( VarId,
    ConsId,
    CommandId,
    ImportDecl (..),
    Program (..),
    SugarProgram (..),
    SugarDecl (..),
    FieldBinding (..),
    MethodDef (..),
    DoThenBinding (..),
    SugarCommand (..),
    HaveBinding (..),
    SugarExpr (..),
    Decl (..),
    Command (..),
    Pattern (..),
    Expr (..),
    Value (..),
    OneHoleContext (..),
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
    freshSubstVar,
    envStoreInsert,
    envStoreInsertCommand,
    envStoreMerge,
    ConsFrame (..),
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

data SugarProgram = SugarProgram
  { sugarProgramImports :: [ImportDecl], -- at beginning
    sugarProgramDecls :: [SugarDecl], -- middle
    sugarProgramExports :: [VarId] -- at end
  }
  deriving (Show, Eq, Ord)

-- | Sugared Declarations | --
data SugarDecl
  = FuncDecl VarId [VarId] SugarCommand -- fn f x y z = e
  | RunDecl SugarCommand
  | DefaultDecl VarId SugarExpr
  | ModuleDecl VarId [FieldBinding] [MethodDef] -- module OBJId := fields methods end
  deriving (Show, Eq, Ord)

-- | Field bindings inside modules | --
data FieldBinding = FieldBinding VarId SugarExpr -- field x = e
  deriving (Show, Eq, Ord)

-- | Method definitions inside modules | --
data MethodDef = MethodDef ConsId [VarId] [VarId] SugarCommand -- MethodId{conts}(args) -> cmd
  deriving (Show, Eq, Ord)

-- | Bindings for do/then syntax | --
data DoThenBinding = Binding Pattern SugarExpr -- x <- e
  deriving (Show, Eq, Ord)

-- | Sugared Commands | --
data SugarCommand
  = LetCommand VarId SugarExpr SugarCommand -- let x = e in q
  | LetcCommand VarId SugarExpr SugarCommand -- letcc x = e in q
  | MatchCommand SugarExpr [(Pattern, SugarCommand)] -- match e with p1 -> q1 | p2 -> q2 | ...
  | PatchCommand SugarExpr [(Pattern, SugarCommand)] -- dispatch e with p1 -> q1 | p2 -> q2 | ...
  | DoThenCommand [DoThenBinding] SugarCommand -- do binding* then q
  | AtCommand SugarExpr [SugarExpr] -- f @ a b c
  | CoAtCommand [SugarExpr] SugarExpr -- a b c @ 'f
  | DotCommand SugarExpr SugarExpr -- p . c
  | ReturnCommand SugarExpr -- return e (used inside methods, desugars to e . _k)
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
  | HaveExpr [HaveBinding] SugarExpr -- have bindings* in e
  | NatLit Integer -- 42
  | TupLit [SugarExpr] -- (e1, e2, ..., en)
  | ListLit [SugarExpr] -- [e1, e2, ..., en]
  | SugarCons ConsId [SugarExpr] -- Foo e1 e2 ... en
  | SugarMu [(Pattern, SugarCommand)] -- mu [ Foo x y -> q | Bar x y -> q | k -> q ]
  | SugarVar VarId -- x
  | SugarDelimExpr SugarCommand -- < ... >
  | ThisExpr VarId -- this.fieldName (desugars to _fieldName inside methods)
  | MethodCall SugarExpr ConsId [SugarExpr] -- obj::Method args or obj::Method{conts}(args)
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
  | DelimExpr Command -- < e . k >
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

-- | Stack Frame for constructor evaluation | --
data ConsFrame = ConsFrame ConsId [Value] [Expr]  -- ConsId, already evaluated args, remaining args
  deriving (Eq, Show, Ord)

-- | Config represents the state of the computation | --
data Config
    -- command to be executed with the current environment and store.
  = CommandConfig Env Store Command
  | ValueConfig Store Value Value -- value and its continuation in the store
    -- constructor evaluation: frame stack, current expr to eval, the context expr
  | ConsEvalConfig Env Store [ConsFrame] Expr Expr
    -- co-constructor evaluation: the evaled half, frame stack, current coexpr to eval
  | CoConsEvalConfig Env Store Value [ConsFrame] Expr
    -- Delimited continuation: the config within, and save the parent config to restore after delimiting
  | DelimConfig Config OneHoleContext
    -- error state with an error message
  | ErrorConfig String -- error message
  deriving (Eq, Show, Ord)

-- | One-hole context for restore from delimited continuations | --
data OneHoleContext = Context VarId Config
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
data Store = Store Addr Addr Int (Map.Map Addr Value) (Map.Map Addr Command)
  deriving (Show, Eq, Ord)

-- | Functions to manipulate the store | --
storeEmpty :: Store
storeEmpty = Store (Addr 0) (Addr 0) 0 Map.empty Map.empty

-- | These functions lookup a value or command in the store by its address | --
storeLookup :: Store -> Addr -> Value
storeLookup (Store _ _ _ store _) x = case Map.lookup x store of
  Just val -> val
  Nothing -> error $ "Value not found in store: " ++ show x

-- | Lookup command in the store | --
storeLookupCommand :: Store -> Addr -> Command
storeLookupCommand (Store _ _ _ _ cmdMap) x = case Map.lookup x cmdMap of
  Just cmd -> cmd
  Nothing -> error $ "Command not found in store: " ++ show x

-- | Allocate a new address for a value or command in the store | --
storeAlloc :: Store -> (Addr, Store)
storeAlloc (Store nextAddr nextCmdAddr counter store _) =
  (nextAddr, Store (addrInc nextAddr) nextCmdAddr counter store Map.empty)

-- | Allocate a new address for a command in the store | --
storeAllocCommand :: Store -> (Addr, Store)
storeAllocCommand (Store nextAddr nextCmdAddr counter store cmdMap) =
  (nextCmdAddr, Store nextAddr (addrInc nextCmdAddr) counter store cmdMap)

-- | Insert a value or command into the store at a specific address | --
storeInsertAddr :: Store -> Addr -> Value -> Store
storeInsertAddr (Store nextAddr nextCmdAddr counter store cmdStore) addr v =
  Store nextAddr nextCmdAddr counter (Map.insert addr v store) cmdStore

storeInsertAddrCommand :: Store -> Addr -> Command -> Store
storeInsertAddrCommand (Store nextAddr nextCmdAddr counter store cmdStore) addr cmd =
  Store nextAddr nextCmdAddr counter store (Map.insert addr cmd cmdStore)

-- | These functions insert a NEW value or command into the store and return the address where it was inserted.
storeInsert :: Store -> Value -> (Addr, Store) -- TODO: use storeInsertAddr
storeInsert (Store nextAddr nextCmdAddr counter store cmdStore) v =
  (nextAddr, Store (addrInc nextAddr) nextCmdAddr counter (Map.insert nextAddr v store) cmdStore)

storeInsertCommand :: Store -> Command -> (Addr, Store)
storeInsertCommand (Store nextAddr nextCmdAddr counter store cmdStore) cmd =
  (nextCmdAddr, Store nextAddr (addrInc nextCmdAddr) counter store (Map.insert nextCmdAddr cmd cmdStore))

-- | Generate a fresh substitution variable for delimited continuations | --
freshSubstVar :: Store -> (VarId, Store)
freshSubstVar (Store nextAddr nextCmdAddr counter store cmdStore) =
  ("_subst" ++ show counter, Store nextAddr nextCmdAddr (counter + 1) store cmdStore)

envStoreInsert :: Env -> Store -> VarId -> Value -> (Env, Store)
envStoreInsert env store var value = (env', store')
  where
    env' = envInsert env var addr
    (addr, store') = storeInsert store value

envStoreMerge :: Env -> Store -> Env -> Store -> (Env, Store)
envStoreMerge env1 store1 (Env env2 cmdEnv2) (Store _ _ _ store2 cmdStore2) =
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
