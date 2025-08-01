module Main (main) where

import Eval (Config (..), step)
import Parser (program)
import Pretty
import Text.Megaparsec (errorBundlePretty, parse)
import Module (evalProgramWithDepDecls)
import Options.Applicative

data RunOptions = RunOptions
  { programFile :: String
  , entryPoint :: String
  , viewEvalProcess :: Bool
  }

runOptions :: Parser RunOptions
runOptions = RunOptions
  <$> strArgument
      ( metavar "PROGRAM_FILE"
        <> help "Path to the program file to evaluate" )
  <*> option str
      ( long "entry-point"
        <> metavar "ENTRY_POINT"
        <> short 'e'
        <> value "main"
        <> showDefault
        <> help "Name of the entry point function" )
  <*> switch
      ( long "step-by-step"
        <> short 's'
        <> help "View evaluation process step by step" )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (runOptions <**> helper)
      ( fullDesc
        <> progDesc "Evaluate a MiniMu program"
        <> header "MiniMu Interpreter" )

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
