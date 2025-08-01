module Main (main) where

import Control.Monad (forM_, when)
import Eval (Config (..), step)
import Module (evalProgramWithDepDecls)
import Options.Applicative
import Parser (program)
import Pretty
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Megaparsec (errorBundlePretty, parse)

data RunOptions = RunOptions
  { programFile :: String,
    entryPoint :: String,
    viewEvalProcess :: Bool
  }

newtype VizOptions = VizOptions
  {vizFile :: String}

data Command
  = Run RunOptions
  | Viz VizOptions
  | Test

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

vizOptions :: Parser VizOptions
vizOptions =
  VizOptions
    <$> strArgument
      ( metavar "PROGRAM_FILE"
          <> help "Path to the program file to visualize"
      )

runCommand :: Parser Command
runCommand = Run <$> runOptions

vizCommand :: Parser Command
vizCommand = Viz <$> vizOptions

testCommand :: Parser Command
testCommand = pure Test

cmdParser :: Parser Command
cmdParser =
  hsubparser
    ( command "run" (info runCommand (progDesc "Run a MiniMu program"))
        <> command "viz" (info vizCommand (progDesc "Visualize a MiniMu program as an Abstract Syntax Tree (AST)"))
        <> command "test-all" (info testCommand (progDesc "Run tests for MiniMu"))
    )
    <|> runCommand

run :: RunOptions -> IO ()
run opts = do
  let file = programFile opts
      entry = entryPoint opts
      view = viewEvalProcess opts
  programText <- readFile file
  programAst <- case parse program file programText of
    Left e -> do
      putStrLn $ errorBundlePretty e
      error "Parse error"
    Right p -> return p
  initConfig <- evalProgramWithDepDecls programAst entry
  -- print config
  let go :: [Config] -> IO ()
      go [] =
        putStrLn "----------------------------------------------------------\nDONE"
      go configs = do
        putStrLn "----------------------------------------------------------"
        mapM_ (putStrLn . renderPretty . prettyConfig) configs
        go (concatMap step configs)
  case view of
    True -> go [initConfig]
    False -> do
      let finalConfig = until isHalted step1 initConfig
      putStrLn "----------------------------------------------------------"
      putStrLn $ renderPretty $ prettyConfig finalConfig
      where
        step1 c = case step c of
          [next] -> next
          _ -> c -- In case of non-determinism, just return the current config
        isHalted (ErrorConfig _) = True
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
  putStrLn $ "Visualizing program: " ++ vizFile opts
  putStrLn "Visualization is not implemented yet."

runTests :: IO ()
runTests = do
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
    let opt = RunOptions fullPath "main" False
    run opt

execCommand :: Command -> IO ()
execCommand (Run opts) = run opts
execCommand (Viz opts) = runViz opts
execCommand Test = runTests

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