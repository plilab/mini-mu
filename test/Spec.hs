import Parser (parseString, parseFile)
import Text.Megaparsec (errorBundlePretty)
import Pretty
import Syntax

main :: IO ()
-- TODO: convert to tests
main = tests

tests :: IO ()
tests = do

  putStrLn "\n TEST PRETTY PRINTER"

  putStrLn "\n variables:"
  printExpr (Var "x")
  printExpr (Var "xyz")
  printExpr (Var "x1")

  putStrLn "\n Co-variables:"
  printCoExpr (CoVar "k")
  printCoExpr (CoVar "k1")

  putStrLn "\n Constructors:"
  printExpr (Cons "Nil" [])
  printExpr (Cons "Pair" [Left (Var "x"), Right (CoVar "k")])
  -- Cons (Head x) (Tail k)
  let nestedCon1 = Cons "Cons" [
                    Left (Cons "Head" [Left (Var "x")]),
                    Right (CoCons "Tail" [Right (CoVar "k")])
                  ]
  printExpr nestedCon1
  let nestedCon2 = Cons "Cons" [
                    Left (Cons "Head" [Left (Var "x")]),
                    Left (Cons "Cons" [
                      Left (Cons "Head" [Left (Var "y")]),
                      Right (CoCons "Tail" [Right (CoVar "k")])
                    ])
                  ]
  printExpr nestedCon2

  putStrLn "\n Command:"
  printCommand (Command (Var "x") (CoVar "k"))
  printCommand (Command (Cons "Cons" [Left (Var "x"), Left (Var "xs")]) (CoVar "k1"))
  printCommand (Command (Cons "Cons" [Left (Var "x"), Left (Var "xs")]) (Mu []))
  printCommand (Command (Cons "Cons" [Left (Var "x"), Left (Var "xs")]) (Mu [
                (VarPattern "x", Command (Var "x") (CoVar "ret")),
                (ConsPattern "Nil" [], Command (Cons "Done" []) (CoVar "ret")),
                (ConsPattern "Cons" [Left "x", Right "xs"],
                 Command (Var "x") (CoCons "Continue" [Right (CoVar "xs")]))
              ]))

  putStrLn "\n Pattern:"
  putStrLn $ showPattern (ConsPattern "Cons" [Left "x", Right "k"])
  putStrLn $ showPattern (VarPattern "abc")

  putStrLn "\n Mu expression:"
  let muExpr = Mu [
                (VarPattern "x", Command (Var "x") (CoVar "ret")),
                (ConsPattern "Nil" [], Command (Cons "Done" []) (CoVar "ret")),
                (ConsPattern "Cons" [Left "x", Right "xs"],
                 Command (Var "x") (CoCons "Continue" [Right (CoVar "xs")]))
              ]
  printCoExpr muExpr

  putStrLn "\n CoMu expression:"
  let comuExpr = CoMu [
                  (CoConsPattern "Print" [Left "x"],
                   Command (Var "x") (CoVar "k")),
                  (CoVarPattern "halt",
                   Command (Cons "Exit" []) (CoVar "k"))
                ]
  printExpr comuExpr

  putStrLn "\n TEST PARSER"
  let input = "< x |> ~k >"
  case parseString input of
    Left err -> putStrLn $ errorBundlePretty err
    Right cmd -> print cmd  
  
  let input1 = "< ListCons x xs |> ~k >"
  case parseString input1 of
    Left err -> putStrLn $ errorBundlePretty err
    Right cmd -> print cmd 

  let input2 = "< ListCons x xs |> mu [ ListNil -> < x |> ~k > | ListCons x xs -> < x |> ~xs > ] >"
  case parseString input2 of
    Left err -> putStrLn $ errorBundlePretty err
    Right cmd -> print cmd

  let sampleAdd = "< comu[ ~Ap3 x y ~k -> < x |> mu[ Z -> < y |> ~k > | S x' -> < add |> ~Ap3 x' (S y) ~k > ] > ] |> ~Ap3 (S Z) (S (S Z)) k0 >"
  case parseString sampleAdd of
    Left err -> putStrLn $ errorBundlePretty err
    Right cmd -> do 
      print cmd
      printCommand cmd
  return ()
