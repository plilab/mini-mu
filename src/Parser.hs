module Parser( parseString, program) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

-- Space consumer
sc :: Parser ()
sc = L.space
  space1                         -- Consume space, newline, tab
  (L.skipLineComment "--")       -- Skip line comments starting with "--"
  (L.skipBlockComment "{-" "-}") -- Skip block comments between "{-" and "-}"

-- Helper for lexemes: consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse a specific symbol and consume trailing whitespace
symbol :: String -> Parser String
symbol = L.symbol sc


-- Parse something between brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- Parse something between angle brackets
angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

-- Parse something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- Parse variable name (starting with lowercase)
varIdentifier :: Parser String
varIdentifier = lexeme $ do
  first <- lowerChar
  rest <- many (alphaNumChar <|> char '\'')
  let word = first : rest
  if word `elem` keywords
    then fail $ "keyword " ++ show word ++ " cannot be the name of a variable"
    else return word
  where
    keywords = ["mu", "comu", "done", "main", "continue"]

-- Parse a constructor (starting with uppercase)
consIdentifier :: Parser String
consIdentifier = lexeme $ do
  first <- upperChar
  rest <- many (alphaNumChar <|> char '\'')
  return (first : rest)

-- varID cannot start with ~
varId :: Parser VarId
varId = lexeme $ do
  notFollowedBy (char '~')
  varIdentifier

-- coVarID must start with ~
coVarId :: Parser CoVarId
coVarId = lexeme $ do
  void (char '~') *> varIdentifier

-- same for constructors
conId :: Parser ConId
conId = lexeme $ do
  notFollowedBy (char '~')
  consIdentifier

coConId :: Parser CoConId
coConId = lexeme $ do
  void (char '~') *> consIdentifier

-- Parse a pattern
pattern :: Parser Pattern
pattern = choice
  [
    -- Parse ConPattern
    try $ do
      c <- conId
      args <- many (choice
        [ Left <$> varId
        , Right <$> coVarId
        ])
      return $ ConPattern c args
    -- Parse VarPattern
    , VarPattern <$> varId
  ]

-- Parse a copattern
coPattern :: Parser CoPattern
coPattern = choice
  [
    -- Parse CoConPattern
    try $ do
      c <- coConId
      args <- many (choice
        [ Left <$> varId
        , Right <$> coVarId
        ])
      return $ CoConPattern c args
    -- Parse CoVarPattern
    , CoVarPattern <$> coVarId
  ]

-- Parse a case pattern -> command in a Mu
patternCase :: Parser (Pattern, Command)
patternCase = do
  pat <- pattern
  _ <- symbol "->"
  cmd <- commandWithAngles
  return (pat, cmd)

-- Parse a case pattern -> command in a CoMu
coPatternCase :: Parser (CoPattern, Command)
coPatternCase = do
  pat <- coPattern
  _ <- symbol "->"
  cmd <- commandWithAngles
  return (pat, cmd)

-- Parse an expression
expr :: Parser Expr
expr = choice
  [
    -- Parse Constructor
    try $ do
      c <- conId
      args <- many (choice
        [ Left <$> expr
        , Right <$> coExpr
        ])
      return $ Con c args
    -- Parse coMu
    , try $ do
      _ <- symbol "comu"
      branches <- brackets $ sepBy1 coPatternCase (symbol "|")
      return $ CoMu branches
    -- handle parentheses
    , try $ parens expr
    -- Parse Variable
    , Var <$> varId
  ]

-- Parse a coexpression
coExpr :: Parser CoExpr
coExpr = choice
  [ try $ do
      c <- coConId
      args <- many (choice
        [ Left <$> expr
        , Right <$> coExpr
        ])
      return $ CoCon c args
  , try $ do
      _ <- symbol "mu"
      branches <- brackets $ sepBy1 patternCase (symbol "|")
      return $ Mu branches
  -- handle parentheses
    , try $ parens coExpr
  , CoVar <$> coVarId
  ]

-- Parse a command
command :: Parser Command
command = do
  e <- expr
  _ <- choice [symbol ">>", symbol "|>"]
  Command e <$> coExpr

-- Parse angle brackets around a command
commandWithAngles :: Parser Command
commandWithAngles = angles command

-- z = e;
-- x = e
-- ~y = co
exprDecl :: Parser Decl
exprDecl = do
  ident <- varId
  _ <- symbol "="
  Decl ident <$> expr

coExprDecl :: Parser Decl
coExprDecl = do
  ident <- coVarId
  _ <- symbol "="
  CoDecl ident <$> coExpr

decl :: Parser Decl
decl = exprDecl <|> coExprDecl

decls :: Parser [Decl]
decls = sepBy1 decl (symbol ";")
  -- (do
  -- d <- decl
  -- semi
  -- ds <- decls
  -- return (d : ds))
  -- <|> (return [])

program :: Parser Program
program = Program <$> (sc *> decls <* eof)

-- Utility function to run the parser
-- TODO: remove parseFile, this is only for testing
parseString :: String -> Either (ParseErrorBundle String Void) Command
parseString = parse (sc *> commandWithAngles <* eof)  ""
