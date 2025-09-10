module Module (evalProgramWithDepDecls) where

import Debug.Trace (trace)
import Eval
import Parser (parseMiniMu)
import Syntax

type ModuleName = String

-- Load a single module by name (assumes .mmu extension)
loadModule :: ModuleName -> IO [Decl]
loadModule moduleName = trace ("loading module \"" ++ moduleName ++ "\"") $ do
  let filepath = "./lib/" ++ moduleName ++ ".mmu"
  program <- parseMiniMu filepath
  -- First, recursively load all dependencies, must be exported declarations
  parentDecls <- buildDeclsFromProgram program
  -- Then, return the declarations from the current module
  let (Program _ decls exports) = program
  let localDecls = filter (\(Decl name _) -> name `elem` exports) decls
  return $ localDecls ++ parentDecls

-- Build module environment by reading imports from a program
buildDeclsFromProgram :: Program -> IO [Decl]
buildDeclsFromProgram (Program imports _ _) = trace ("building module env for imports: " ++ show imports) $ do
  let moduleNames = [name | ImportDecl name _ <- imports]
  results <- mapM loadModule (removeDuplicates moduleNames)
  return $ concat results
  where
    removeDuplicates [] = []
    removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- Evaluate program with automatic dependency resolution
evalProgramWithDepDecls :: Program -> VarId -> IO Config
evalProgramWithDepDecls prog@(Program _ decls _) varId = do
  -- Build module environment from program's imports
  allDecls <- buildDeclsFromProgram prog
  let (loadedEnv, loadedStore) = evalDecls initEnv initStore allDecls
  return $
    uncurry
      CommandConfig
      (evalDecls loadedEnv loadedStore decls)
      (Command (Var varId) halt)
  where
    halt = Cons "Halt" []
