{-# LANGUAGE OverloadedStrings #-}

module Tests (main) where

import Control.Monad (forM_, when)
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Process (callCommand)

filesDir :: FilePath
filesDir = "./test"

main :: IO ()
main = do
  putStrLn "Building the project..."
  callCommand "stack build"

  putStrLn $ "Looking for .mmu files in: " ++ filesDir
  allFiles <- listDirectory filesDir
  let mmuFiles = filter (\f -> takeBaseName f /= "temp") $ filter (\f -> takeExtension f == ".mmu") allFiles

  when (null mmuFiles) $ do
    putStrLn "No .mmu files found."
    exitFailure

  forM_ mmuFiles $ \file -> do
    let fullPath = filesDir </> file
    putStrLn $ "\nRunning: stack exec mini-mu " ++ fullPath
    callCommand $ "stack exec mini-mu " ++ fullPath ++ " main"
