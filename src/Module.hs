module Module (main, evalProgramWithDepDecls) where

import Debug.Trace (trace)
import Eval
import Parser (parseFile)
import Pretty
import Syntax

type ModuleName = String

-- Load a single module by name (assumes .mmu extension)
loadModule :: ModuleName -> IO [Decl]
loadModule moduleName = trace ("loading module \"" ++ moduleName ++ "\"") $ do
  let filepath = "./test/" ++ moduleName ++ ".mmu"
  parseResult <- parseFile filepath
  case parseResult of
    Left err -> error $ "Error parsing module file: " ++ show err
    Right program -> do
      -- First, recursively load all dependencies
      parentDecls <- buildDeclsFromProgram program
      -- Then, return the declarations from the current module
      return $ concat [decls | Program _ decls _ <- [program]] ++ parentDecls
      

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

-- Simple function to run a program file with automatic dependency resolution
runProgram :: FilePath -> VarId -> IO (Either String Config)
runProgram filepath varId = do
  parseResult <- parseFile filepath
  case parseResult of
    Left err -> return $ Left $ show err
    Right program -> do
      config <- evalProgramWithDepDecls program varId
      return $ Right config

main :: IO ()
main = do
  result <-
    runProgram
      "./test/quicksort_sugared.mmu"
      "main"
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right config -> do
      let finalConfig = until isHalted step1 config
      putStrLn "----------------------------------------------------------"
      putStrLn $ renderPretty $ prettyConfig finalConfig
      where
        step1 c = case step c of
          [next] -> next
          _ -> c -- In case of non-determinism, just return the current config
        isHalted (ErrorConfig _) = True
        isHalted _ = False