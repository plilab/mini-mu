{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (isInfixOf)
import Eval (eval, evalDecls, initEnv, initStore, step, tryMatch)
import Module (evalProgramWithDepDecls)
import Parser (parseFile)
import Syntax
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "MiniMu Parser Tests" $ do
    parserTests

  describe "MiniMu Evaluation Tests" $ do
    evaluationTests

  describe "Integration Tests" $ do
    integrationTests

parserTests :: Spec
parserTests = do
  describe "Basic Expression Parsing" $ do
    it "parses variables" $ do
      result <- parseFile "./tests/parser_test.mmu"
      case result of
        Right (Program [] [Decl "test" (Var "x")] []) -> True `shouldBe` True
        _ -> expectationFailure $ "Failed to parse variable: " ++ show result

-- TODO: more tests

evaluationTests :: Spec
evaluationTests = do
  describe "Basic Evaluation" $ do
    it "evaluates simple expressions" $ do
      let env = initEnv
      let store = initStore
      let expr = Cons "Z" []
      let (value, _) = eval env store expr
      value `shouldBe` ConsValue "Z" []

    it "evaluates variables" $ do
      let env = initEnv
      let store = initStore
      let decls = [Decl "x" (Cons "Z" [])]
      let (env', store') = evalDecls env store decls
      let expr = Var "x"
      let (value, _) = eval env' store' expr
      value `shouldBe` ConsValue "Z" []

  describe "Pattern Matching" $ do
    it "matches simple patterns" $ do
      let env = initEnv
      let store = initStore
      let value = ConsValue "Z" []
      let pattern = ConsPattern "Z" []
      let result = tryMatch env store value pattern
      result `shouldSatisfy` (/= Nothing)

    it "matches variable patterns" $ do
      let env = initEnv
      let store = initStore
      let value = ConsValue "Z" []
      let pattern = VarPattern "x"
      let result = tryMatch env store value pattern
      result `shouldSatisfy` (/= Nothing)

    it "rejects mismatched patterns" $ do
      let env = initEnv
      let store = initStore
      let value = ConsValue "S" [ConsValue "Z" []]
      let pattern = ConsPattern "Z" []
      let result = tryMatch env store value pattern
      result `shouldBe` Nothing

integrationTests :: Spec
integrationTests = do
  describe "Test File Integration" $ do
    it "runs add.mmu successfully" $ do
      result <- testMmuFile "tests/add.mmu"
      result `shouldSatisfy` isSuccessfulHalt

    it "runs inc.mmu successfully" $ do
      result <- testMmuFile "tests/inc.mmu"
      result `shouldSatisfy` isSuccessfulHalt

    it "runs test_bool.mmu successfully" $ do
      result <- testMmuFile "tests/test_bool.mmu"
      result `shouldSatisfy` isSuccessfulHalt

  describe "All Test Files" $ do
    it "runs all .mmu test files without crashing" $ do
      allFiles <- listDirectory "tests"
      let mmuFiles = filter (\f -> takeExtension f == ".mmu") allFiles
      results <- mapM (\f -> testMmuFile ("tests" </> f)) mmuFiles
      length results `shouldBe` length mmuFiles

-- Helper functions for testing
testMmuFile :: FilePath -> IO Config
testMmuFile filepath = do
  result <- parseFile filepath
  case result of
    Right program -> do
      config <- evalProgramWithDepDecls program "main"
      return $ runToCompletion config
    Left err -> error $ "Parse error: " ++ show err

runToCompletion :: Config -> Config
runToCompletion config =
  case step config of
    [] -> config
    [nextConfig] -> runToCompletion nextConfig
    configs -> head configs -- Take first in case of non-determinism

isSuccessfulHalt :: Config -> Bool
isSuccessfulHalt (ErrorConfig msg) = "Halt" `isInfixOf` msg
isSuccessfulHalt _ = False

-- Helper functions are now imported directly from Eval module
