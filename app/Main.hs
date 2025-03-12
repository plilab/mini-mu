module Main where

import Pretty
import Syntax

main :: IO ()
main = do

  putStrLn "\n variables:"
  printExpr (Var "x")
  printExpr (Var "xyz")
  printExpr (Var "x1")

  putStrLn "\n Co-variables:"
  printCoExpr (CoVar "k")
  printCoExpr (CoVar "k1")

  putStrLn "\n Constructors:"
  printExpr (Con "Nil" [])
  printExpr (Con "Pair" [Left (Var "x"), Right (CoVar "k")])
  let nestedCon1 = Con "Cons" [
                    Left (Con "Head" [Left (Var "x")]),
                    Right (CoCon "Tail" [Right (CoVar "k")])
                  ]
  printExpr nestedCon1
  let nestedCon2 = Con "Cons" [
                    Left (Con "Head" [Left (Var "x")]),
                    Left (Con "Cons" [
                      Left (Con "Head" [Left (Var "y")]),
                      Right (CoCon "Tail" [Right (CoVar "k")])
                    ])
                  ]
  printExpr nestedCon2

  putStrLn "\n Command:"
  printCommand (Command (Var "x") (CoVar "k"))
  printCommand (Command (Con "Cons" [Left (Var "x"), Left (Var "xs")]) (CoVar "k1"))
  printCommand (Command (Con "Cons" [Left (Var "x"), Left (Var "xs")]) (Mu []))
  printCommand (Command (Con "Cons" [Left (Var "x"), Left (Var "xs")]) (Mu [
                (VarPattern "x", Command (Var "x") (CoVar "ret")),
                (ConPattern "Nil" [], Command (Con "Done" []) (CoVar "ret")),
                (ConPattern "Cons" [Left "x", Right "xs"],
                 Command (Var "x") (CoCon "Continue" [Right (CoVar "xs")]))
              ]))

  putStrLn "\n Pattern:"
  putStrLn $ showPattern (ConPattern "Cons" [Left "x", Right "k"])
  putStrLn $ showPattern (VarPattern "abc")

  putStrLn "\n Mu expression:"
  let muExpr = Mu [
                (VarPattern "x", Command (Var "x") (CoVar "ret")),
                (ConPattern "Nil" [], Command (Con "Done" []) (CoVar "ret")),
                (ConPattern "Cons" [Left "x", Right "xs"],
                 Command (Var "x") (CoCon "Continue" [Right (CoVar "xs")]))
              ]
  printCoExpr muExpr

  putStrLn "\n CoMu expression:"
  let comuExpr = CoMu [
                  (CoConPattern "Print" [Left "x"],
                   Command (Var "x") (CoVar "k")),
                  (CoVarPattern "halt",
                   Command (Con "Exit" []) (CoVar "k"))
                ]
  printExpr comuExpr

