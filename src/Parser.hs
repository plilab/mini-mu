module Parser (parseString, parseFile, program) where

import Data.Void
import Debug.Trace
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Space consumer
sc :: Parser ()
sc =
  L.space
    space1 -- Consume space, newline, tab
    (L.skipLineComment "--") -- Skip line comments starting with "--"
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
varIdentifier =
  label
    "variable name"
    ( lexeme $ do
        first <- lowerChar
        rest <- many (alphaNumChar <|> char '_' <|> char '\'')
        let word = first : rest
        if word `elem` keywords
          then fail $ "keyword " ++ show word ++ " cannot be the name of a variable"
          else return word
    )
  where
    keywords = ["mu"]

-- Parse a constructor (starting with uppercase)
consIdentifier :: Parser String
consIdentifier =
  label
    "constructor name"
    ( lexeme $ do
        first <- upperChar
        rest <- many (alphaNumChar <|> char '_' <|> char ':' <|> char '\'')
        return (first : rest)
    )

-- varID cannot start with ~
varId :: Parser VarId
varId =
  label
    "variable id"
    ( lexeme $ do
        -- notFollowedBy (char '~')
        varIdentifier
    )

-- coVarID must start with ~
-- coVarId :: Parser CoVarId
-- coVarId =
--   label
--     "co-variable id"
--     ( lexeme $ do
--         void (char '~') *> varIdentifier
--     )

-- same for constructors
consId :: Parser ConsId
consId =
  label
    "constructor id"
    ( lexeme $ do
        -- notFollowedBy (char '~')
        consIdentifier
    )

-- coConsId :: Parser CoConsId
-- coConsId =
--   label
--     "co-constructor id"
--     ( lexeme $ do
--         void (char '~') *> consIdentifier
--     )

-- Parse a pattern
pattern :: Parser Pattern
pattern =
  label
    "pattern"
    ( choice
        [ try $ do
            c <- consId
            args <- many varId              
            return $ ConsPattern c args,
          -- Parse VarPattern
          try $ VarPattern <$> varId
        ]
    )

-- Parse a copattern
-- coPattern :: Parser CoPattern
-- coPattern =
--   label
--     "co-pattern"
--     ( choice
--         [ try $ do
--             c <- coConsId
--             args <-
--               many
--                 ( choice
--                     [ Left <$> varId,
--                       Right <$> coVarId
--                     ]
--                 )
--             return $ CoConsPattern c args,
--           -- Parse CoVarPattern
--           try $ CoVarPattern <$> coVarId
--         ]
--     )

-- Parse a case pattern -> command in a Mu
patternCase :: Parser (Pattern, Command)
patternCase =
  label
    "pattern case"
    ( do
        pat <- pattern
        _ <- symbol "->"
        cmd <- commandWithAngles
        return (pat, cmd)
    )

-- Parse a case pattern -> command in a CoMu
-- coPatternCase :: Parser (CoPattern, Command)
-- coPatternCase =
--   label
--     "co-pattern case"
--     ( do
--         pat <- coPattern
--         _ <- symbol "->"
--         cmd <- commandWithAngles
--         return (pat, cmd)
--     )

atom :: Parser Expr
atom = trace "Parsing atom" atomExpr

atomExpr :: Parser Expr
atomExpr =
  label
    "atom expr"
    ( choice
        [ try consWithNoArgs,
          exprAux
        ]
    )

-- atomCoExpr :: Parser CoExpr
-- atomCoExpr =
--   trace "Parsing atom coexpr" $
--     label
--       "atom coexpr"
--       ( choice
--           [ try coConsWithNoArgs,
--             coExprAux
--           ]
--       )

consWithNoArgs :: Parser Expr
consWithNoArgs = do
  c <- consId
  return $ Cons c []

-- coConsWithNoArgs :: Parser CoExpr
-- coConsWithNoArgs = do
--   c <- coConsId
--   return $ CoCons c []

expr :: Parser Expr
expr =
  label
    "expression"
    ( choice
        [ try $ Cons <$> consId <*> many atom,
          exprAux
        ]
    )

exprAux :: Parser Expr
exprAux =
  label
    "expression auxiliary"
    ( choice
        [ try $ do
            _ <- symbol "mu"
            branches <- brackets $ sepBy1 patternCase (symbol "|")
            return $ Mu branches,
          try $ Var <$> varId,
          try $ parens expr
        ]
    )

-- Parse a coexpression
-- coExpr :: Parser CoExpr
-- coExpr =
--   label
--     "co-expression"
--     ( choice
--         [ try $ do
--             _ <- symbol "mu"
--             branches <- brackets $ sepBy1 patternCase (symbol "|")
--             return $ Mu branches,
--           try $ CoCons <$> coConsId <*> many atom,
--           try $ CoVar <$> coVarId,
--           -- handle parentheses
--           try $ parens coExpr
--         ]
--     )

-- Parse a command
command :: Parser Command
command =
  label
    "command"
    ( do
        e <- expr
        _ <- choice [symbol ">>", symbol "|>"]
        Command e <$> expr
    )

-- Parse angle brackets around a command
commandWithAngles :: Parser Command
commandWithAngles = angles command

-- z = e;
-- x = e
-- ~y = co
decl :: Parser Decl
decl =
  label
    "Declaration"
    ( do
        ident <- varId
        _ <- symbol "="
        Decl ident <$> expr
    )

-- coExprDecl :: Parser Decl
-- coExprDecl =
--   label
--     "co-expression declaration"
--     ( do
--         ident <- coVarId
--         _ <- symbol "="
--         CoDecl ident <$> coExpr
--     )

decls :: Parser [Decl]
decls = sepBy1 decl (symbol ";")

-- (do
-- d <- decl
-- semi
-- ds <- decls
-- return (d : ds))
-- <|> (return [])

program :: Parser Program
program = label "program" (Program <$> (sc *> decls <* eof))

-- Utility function to run the parser

-- Parse a string into a Command, consuming leading whitespace, this is used for testing
parseString :: String -> Either (ParseErrorBundle String Void) Command
parseString = parse (sc *> commandWithAngles <* eof) ""

-- Parse a file into a Program, consuming leading whitespace
parseFile :: String -> IO (Either (ParseErrorBundle String Void) Program)
parseFile file = do
  contents <- readFile file
  return $ parse (sc *> program <* eof) file contents
