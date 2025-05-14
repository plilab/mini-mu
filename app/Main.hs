module Main (main) where

import Eval (evalProgram)
import Graph (graphMain)
import Parser (program, parseString)
import Pretty
import Syntax

import System.Environment
import Text.Megaparsec (parse, errorBundlePretty)

main :: IO ()
main = do
  -- tests
  -- read command line args: args <- getArgs
  [programFile, var] <- getArgs
  programText <- readFile programFile
  programAst <- case parse program programFile programText of
        Left e -> do
          putStrLn $ errorBundlePretty e
          error "Parse error"
        Right p -> return p
  let config = evalProgram programAst var
  print config
  -- graph = [evalProg on parsed file]
  -- while notDone:
  --   newGraph = [for src in graph:
  --     for dst in apply step to src:
  --       edge from src to dst]
  --   if newGraph == graph: stop loop
  --   else: print graph with GraphViz and loop again
  return ()

-- TODO: convert to tests
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
  printExpr (Con "Nil" [])
  printExpr (Con "Pair" [Left (Var "x"), Right (CoVar "k")])
  -- Cons (Head x) (Tail k)
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
  
  graphMain
{-

f = (\x . f x)

v = mu[ k -> <v >> k > ] -- eta for mu

inf = mu[ k -> < S(inf) >> k > ]
inf' = S(mu[ k -> < S(inf) >> k > ])

zero = Z
one = S(zero)
two = S(one)

data Nat = S Nat | Z

plus 0 y = y
plus (S x) y = S(add x y) -- add x (S y)

add = mu[v k -> x $ v | ]
add = mu[Ap3 x y k ->
  x $ mu [
    Z -> y $ k
  | S x' -> add $ Ap3 x' (S y) k
  ]
]
add = mu[Ap3 x y k ->
  x $ mu [
    Z -> y $ k
  | S x' -> add $ Ap3 x' y mu[v -> S v | k]
  ]
]

-- https://github.com/duo-lang/duo-lang/blob/263c050bd650b4b21f2c71fc45a0b6961a023b4a/std/Codata/Function.duo#L44
def prd fixLazy := cocase { Ap(f,k) =>
                    cocase { Ap(x,k) => x >> Ap(x,mu xx. f >> Ap(xx,k)) } >>
                 Ap(cocase { Ap(x,k) => x >> Ap(x,mu xx. f >> Ap(xx,k)) },k)};
def prd omega := mu k.fixLazy >> Ap(id,k) ;
omega = (\x -> x x) (\x -> x x)


def prd omega :=
                    cocase { Ap(x,k) => x >> Ap(x,k) } >>
                 Ap(cocase { Ap(x,k) => x >> Ap(x,k) },k)

                    cocase { Ap(x,k) => x >> Ap(x,k) } >>
                 Ap(cocase { Ap(x,k) => x >> Ap(x,k) },k)

omega = mu[Ap x k -> x $ Ap x k] $ Ap (mu[Ap x k -> x $ Ap x k]) k
fix = mu[Ap f k => mu[Ap x k => x $ Ap x (mu [xx -> f $ Ap xx k])]] >>
  Ap (mu[Ap f k => mu[Ap x k => x $ Ap x (mu [xx -> f $ Ap xx k])]] k

data F = F (-F)
omega := mu[F k -> F k $ k] $ F (mu[F k -> F k $ k])

>> |>
      (mu[Ap x -> x >> Ap x]) $ Ap (mu[Ap x -> x $ Ap x])
mu[Ap x -> x $ Ap x] $ Ap (mu[Ap x -> x $ Ap x])


mu[Ap x k -> x $ k] $ Ap (mu[Ap x k -> x $ k]) ?
=>
x $ 
mu[Ap x k -> x $ k] $ Ap (mu[Ap x k -> x $ k]) ?
==> fail

mu[k -> k $ k] $ mu[x -> ??]
=>

< inc | Ap inf k >

inc = mu[Ap x k -> < S(x) |> k >]

< inc | Ap inf (mu[v -> <inc | App v k>]) >
==>
< mu[Ap x k -> < S(x) |> k >] | Ap (mu[ k -> < S(inf) |> k > ]) (mu[v -> <inc | App v k>]) >
==>
< S(mu[ k -> < S(inf) |> k > ]) |> (mu[v -> <inc | App v k>]) >
==>
<inc | Ap (S(mu[ k -> < S(inf) |> k > ])) k>
==>
<S(S(mu[ k -> < S(inf) |> k > ])) |> k >

dec = mu[Ap x k -> <x | mu[S(y) -> <y | k>]>]
<dec | App inf k0>
==>        
<mu[Ap x k -> <x | mu[S(y) -> <y | k>]>] | App mu[ k -> < S(inf) |> k > ] k0>
==>
<mu[ k -> < S(inf) |> k > ] | mu[S(y) -> <y | k0>]>
==>
< S(inf) |> mu[S(y) -> <y | k0>] >
==>
< S(mu[ k -> < S(inf) |> k > ]) |> mu[S(y) -> <y | k0>] >
==>
<mu[ k -> < S(inf) |> k > ] | k0>



-}
