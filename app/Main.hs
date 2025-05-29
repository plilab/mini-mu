module Main (main) where

import Eval (evalProgram, step, Config)
-- import Graph (graphMain)
import Parser (program)

import System.Environment
import Text.Megaparsec (parse, errorBundlePretty)

main :: IO ()
main = do
  -- read command line args: args <- getArgs
  [programFile, var] <- getArgs
  programText <- readFile programFile
  programAst <- case parse program programFile programText of
        Left e -> do
          putStrLn $ errorBundlePretty e
          error "Parse error"
        Right p -> return p
  let config = evalProgram programAst var
  -- print config
  let go :: [Config] -> IO ()
      go [] = putStrLn "----\nDONE"
      go configs = do
        putStrLn "----"
        mapM_ print configs
        go (concatMap step configs)
  go [config]
  -- graph = [evalProg on parsed file]
  -- while notDone:
  --   newGraph = [for src in graph:
  --     for dst in apply step to src:
  --       edge from src to dst]
  --   if newGraph == graph: stop loop
  --   else: print graph with GraphViz and loop again
  return ()
