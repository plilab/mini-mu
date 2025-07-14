module Main (main) where

import Eval (Config (..), evalProgram, step)
-- import Graph (graphMain)
import Parser (program)
import Pretty
import System.Environment
import Text.Megaparsec (errorBundlePretty, parse)
import Module (evalProgramWithDepDecls)

main :: IO ()
main = do
  -- read command line args: args <- getArgs
  [programFile, var, viewEvalProcess] <- getArgs
  programText <- readFile programFile
  programAst <- case parse program programFile programText of
    Left e -> do
      putStrLn $ errorBundlePretty e
      error "Parse error"
    Right p -> return p
  initConfig <- evalProgramWithDepDecls programAst var
  -- print config
  let go :: [Config] -> IO ()
      go [] = 
        putStrLn "----------------------------------------------------------\nDONE"
      go configs = do
        putStrLn "----------------------------------------------------------"
        mapM_ (putStrLn . renderPretty . prettyConfig) configs
        go (concatMap step configs)
  case viewEvalProcess of
    "true" -> go [initConfig]
    _ -> do
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
