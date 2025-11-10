module Main (main) where

import Control.Monad (forM_, when)
import Eval (Config (..), step)
import EvalTree (generateEvalTreeWithDepth, printEvalTree)
import Graph (visualizeEvalTree)
import Module (evalProgramWithDepDecls)
import Options.Applicative
import Parser (parseMiniMu)
import Pretty
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeExtension, (</>))

data RunOptions = RunOptions
  { programFile :: String,
    entryPoint :: String,
    viewEvalProcess :: Bool,
    viewFullEnvRun :: Bool,
    standardRun :: Bool
  }

data VizOptions = VizOptions
  { vizFile :: String,
    pretty :: Bool
  }

data TreeOptions = TreeOptions
  { treeFile :: String,
    treeEntryPoint :: String,
    treeDepth :: Int,
    outputFormat :: String,
    viewFullEnvTree :: Bool
  }

newtype TestOptions = TestOptions {standardTest :: Bool}

data UserCommand
  = Run RunOptions
  | Viz VizOptions
  | Tree TreeOptions
  | Test TestOptions

runOptions :: Parser RunOptions
runOptions =
  RunOptions
    <$> strArgument
      ( metavar "PROGRAM_FILE"
          <> help "Path to the program file to evaluate"
      )
    <*> option
      str
      ( long "entry-point"
          <> metavar "ENTRY_POINT"
          <> short 'e'
          <> value "main"
          <> showDefault
          <> help "Name of the entry point - the expression to evaluate"
      )
    <*> switch
      ( long "step-by-step"
          <> short 's'
          <> help "View evaluation process step by step"
      )
    <*> switch
      ( long "full-env"
          <> short 'f'
          <> help "View evaluation environment"
      )
    <*> switch
      ( long "standard"
          <> short 'r'
          <> help "Record this run as standard"
      )

vizOptions :: Parser VizOptions
vizOptions =
  VizOptions
    <$> strArgument
      ( metavar "PROGRAM_FILE"
          <> help "Path to the program file to visualize"
      )
    <*> switch
      ( long "pretty"
          <> short 'p'
          <> help "Pretty print the AST"
      )

treeOptions :: Parser TreeOptions
treeOptions =
  TreeOptions
    <$> strArgument
      ( metavar "PROGRAM_FILE"
          <> help "Path to the program file to generate evaluation tree for"
      )
    <*> option
      str
      ( long "entry-point"
          <> metavar "ENTRY_POINT"
          <> short 'e'
          <> value "main"
          <> showDefault
          <> help "Name of the entry point - the expression to evaluate"
      )
    <*> option
      auto
      ( long "depth"
          <> metavar "DEPTH"
          <> short 'd'
          <> value 10
          <> showDefault
          <> help "Maximum depth of the evaluation tree"
      )
    <*> option
      str
      ( long "output"
          <> metavar "FORMAT"
          <> short 'o'
          <> value "text"
          <> showDefault
          <> help "Output format: text or svg"
      )
    <*> switch
      ( long "full-env"
          <> short 'f'
          <> help "View full evaluation environment"
      )

testOptions :: Parser TestOptions
testOptions =
  TestOptions
    <$> switch
      ( long "standard"
          <> short 'r'
          <> help "Record this test as standard"
      )

runCommand :: Parser UserCommand
runCommand = Run <$> runOptions

vizCommand :: Parser UserCommand
vizCommand = Viz <$> vizOptions

treeCommand :: Parser UserCommand
treeCommand = Tree <$> treeOptions

testCommand :: Parser UserCommand
testCommand = Test <$> testOptions

cmdParser :: Parser UserCommand
cmdParser =
  hsubparser
    ( command "run" (info runCommand (progDesc "Run a MiniMu program"))
        <> command "viz" (info vizCommand (progDesc "Visualize a MiniMu program as an Abstract Syntax Tree (AST)"))
        <> command "tree" (info treeCommand (progDesc "Generate evaluation tree for a MiniMu program"))
        <> command "test-all" (info testCommand (progDesc "Run tests for MiniMu"))
    )
    <|> runCommand

run :: RunOptions -> IO ()
run opts = do
  let file = programFile opts
      entry = entryPoint opts
      view = viewEvalProcess opts
      viewEnv = viewFullEnvRun opts
      standard = standardRun opts

  programAst <- parseMiniMu file

  when standard $ do
    putStrLn $ "Recording this run as standard for " ++ file
    -- Save AST to ./tests/ast/*.mmu
    let baseFileName = takeBaseName file
        astFile = "tests/ast/" ++ baseFileName ++ ".mmu.ast"
    writeFile astFile (show programAst)
    -- IF we want to pretty print the AST then change to this:
    -- writeFile astFile (renderPretty $ prettyProgram programAst)
    putStrLn $ "AST saved to " ++ astFile

  initConfig <- evalProgramWithDepDecls programAst entry
  -- print config
  let go :: String -> Bool -> [Config] -> IO ()
      go fileName save [] = do
        let message = "----------------------------------------------------------\nDONE"
        putStrLn message
        when save $ do
          appendFile fileName message
          putStrLn $ "Config graph saved to " ++ fileName
      go fileName save configs = do
        let message = "----------------------------------------------------------"
        putStrLn message
        mapM_ (putStrLn . renderPretty . flip prettyConfig viewEnv) configs
        when save $ do
          appendFile fileName (message ++ "\n")
          mapM_ (appendFile fileName . (++ "\n") . renderPretty . flip prettyConfig viewEnv) configs

        go fileName save (concatMap step configs)

  case view of
    True -> do
      let baseFileName = takeBaseName file
          outFile = "tests/out/" ++ baseFileName ++ ".mmu.out"
      writeFile outFile "" -- clear the file before writing
      go outFile True [initConfig]
    False -> do
      let finalConfigs = until isHalted step1 [initConfig]
      putStrLn "----------------------------------------------------------"
      mapM_ (putStrLn . renderPretty . flip prettyConfig viewEnv) finalConfigs
      when (isHalted finalConfigs) $
        putStrLn "Evaluation halted."

      -- Save final config to ./tests/out/*.mmu if standard run
      when standard $ do
        let baseFileName = takeBaseName file
            outFile = "tests/out/" ++ baseFileName ++ ".mmu.out"
        writeFile outFile (concatMap (renderPretty . flip prettyConfig viewEnv) finalConfigs)
        putStrLn $ "Final config saved to " ++ outFile
      where
        step1 :: [Config] -> [Config]
        step1 = concatMap step

        isHalted :: [Config] -> Bool
        isHalted ((ErrorConfig _) : _) = True
        isHalted _ = False
  -- graph = [evalProg on parsed file]
  -- while notDone:
  --   newGraph = [for src in graph:
  --     for dst in apply step to src:
  --       edge from src to dst]
  --   if newGraph == graph: stop loop
  --   else: print graph with GraphViz and loop again
  return ()

runViz :: VizOptions -> IO ()
runViz opts = do
  program <- parseMiniMu $ vizFile opts
  let prettyPrint = pretty opts
  if prettyPrint
    then mapM_ (putStrLn . renderPretty . prettyProgram) [program]
    else mapM_ print [program]

runTree :: TreeOptions -> IO ()
runTree opts = do
  let file = treeFile opts
      entry = treeEntryPoint opts
      depth = treeDepth opts
      format = outputFormat opts
      fullEnv = viewFullEnvTree opts
  programAst <- parseMiniMu file
  initConfig <- evalProgramWithDepDecls programAst entry
  case format of
    "svg" -> do
      let baseFileName = takeBaseName file
          outputFile = "tests/svg/" ++ baseFileName ++ ".mmu.svg"
      visualizeEvalTree initConfig depth fullEnv outputFile
    "text" -> do
      let tree = generateEvalTreeWithDepth depth initConfig
      putStrLn $ printEvalTree tree fullEnv
    _ -> putStrLn "Invalid output format. Use 'text' or 'svg'."

runTests :: TestOptions -> IO ()
runTests opts = do
  let asStandard = standardTest opts
  let filesDir = "./tests"
  putStrLn $ "Looking for .mmu files in: " ++ filesDir
  allFiles <- listDirectory filesDir
  let mmuFiles = filter (\f -> takeBaseName f /= "temp") $ filter (\f -> takeExtension f == ".mmu") allFiles

  when (null mmuFiles) $ do
    putStrLn "No .mmu files found."
    exitFailure

  forM_ mmuFiles $ \file -> do
    let fullPath = filesDir </> file
    putStrLn $ "\nRunning: stack exec mini-mu " ++ fullPath
    let opt = RunOptions fullPath "main" False False asStandard
    run opt

execCommand :: UserCommand -> IO ()
execCommand (Run opts) = run opts
execCommand (Viz opts) = runViz opts
execCommand (Tree opts) = runTree opts
execCommand (Test opts) = runTests opts

main :: IO ()
main = execCommand =<< execParser opts
  where
    opts =
      info
        (cmdParser <**> helper)
        ( fullDesc
            <> progDesc "A set of tools to use and analyze the MiniMu language"
            <> header "MiniMu CLI"
        )
