module Parser () where

import Syntax

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol = void . L.symbol sc

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- Parser for VarId
varId :: Parser VarId
varId = identifier

-- Parser for ConId
conId :: Parser ConId
conId = identifier

-- Parser for CoVarId
coVarId :: Parser CoVarId
coVarId = identifier

-- Parser for CoConId
coConId :: Parser CoConId
coConId = identifier

-- Parser for Command
command :: Parser Command
command = do
  symbol "<"
  e <- expr
  symbol "|"
  k <- coExpr
  symbol ">"
  return $ Command e k

-- Parser for Pattern
pattern :: Parser Pattern
pattern = try conPattern <|> varPattern
  where
    conPattern = do
      c <- conId
      args <- many (try (Left <$> varId) <|> (Right <$> coVarId))
      return $ ConPattern c args
    varPattern = VarPattern <$> varId

-- Parser for Expr
expr :: Parser Expr
expr = makeExprParser term operatorTable
  where
    operatorTable = [] -- Empty table for now, add operators as needed
    term = choice
      [ Var <$> varId
      , conExpr
      , coMuExpr
      ]
    conExpr = do
      c <- conId
      args <- many (try (Left <$> expr) <|> (Right <$> coExpr))
      return $ Con c args
    coMuExpr = do
      symbol "mu"
      symbol "["
      clauses <- sepBy clause (symbol "|")
      symbol "]"
      return $ CoMu clauses
    clause = do
      p <- coPattern
      symbol "->"
      q <- command
      return (p, q)

-- Parser for CoPattern
coPattern :: Parser CoPattern
coPattern = try coConPattern <|> coVarPattern
  where
    coConPattern = do
      c <- coConId
      args <- many (try (Left <$> varId) <|> (Right <$> coVarId))
      return $ CoConPattern c args
    coVarPattern = CoVarPattern <$> coVarId

-- Parser for CoExpr
coExpr :: Parser CoExpr
coExpr = makeExprParser term operatorTable
  where
    operatorTable = [] -- Empty table for now, add operators as needed
    term = choice
      [ CoVar <$> coVarId
      , coConExpr
      , muExpr
      ]
    coConExpr = do
      c <- coConId
      args <- many (try (Left <$> expr) <|> (Right <$> coExpr))
      return $ CoCon c args
    muExpr = do
      symbol "mu"
      symbol "["
      clauses <- sepBy clause (symbol "|")
      symbol "]"
      return $ Mu clauses
    clause = do
      p <- pattern
      symbol "->"
      q <- command
      return (p, q)
