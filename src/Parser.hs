module Parser
  ( parseString,
    parseFile,
    program,
    Parser,
    expr,
    symbol,
    brackets,
    angles,
    parens,
    varIdentifier,
    consIdentifier,
    varId,
    consId,
    command,
    decl,
    pattern,
  )
where

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
    $ lexeme
    $ do
      first <- lowerChar
      rest <- many (alphaNumChar <|> char '_' <|> char '\'')
      let word = first : rest
      if word `elem` keywords
        then fail $ "keyword " ++ show word ++ " cannot be the name of a variable"
        else return word
  where
    keywords = ["mu", "def", "run", "let", "in", "where", "do", "then"]

-- Parse a constructor (starting with uppercase)
consIdentifier :: Parser String
consIdentifier =
  label
    "constructor name"
    $ lexeme
    $ do
      first <- upperChar
      rest <- many (alphaNumChar <|> char '_' <|> char ':' <|> char '\'')
      return (first : rest)

nat :: Parser Expr
nat = label "natural number" $ lexeme $ do
  intToPeano <$> L.decimal
  where
    intToPeano :: Integer -> Expr
    intToPeano 0 = Cons "Z" []
    intToPeano n = Cons "S" [intToPeano (n - 1)]

-- varID cannot start with ~
varId :: Parser VarId
varId =
  label
    "variable id"
    $ lexeme
    $ do
      -- notFollowedBy (char '~')
      varIdentifier

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
    $ choice
      [ try $ do
          c <- consId
          args <- many varId
          return $ ConsPattern c args,
        -- Parse VarPattern
        try $ VarPattern <$> varId
      ]

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
    $ do
      pat <- pattern
      _ <- symbol "->"
      cmd <- command
      return (pat, cmd)

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
  trace "Parsing atom expr"
    $ label
      "atom expr"
    $ choice
      [ try consWithNoArgs,
        try nat,
        exprAux
      ]

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
  trace "Parsing expr"
    $ label
      "expression"
    $ choice
      [ try letExpr,
        try $ Cons <$> consId <*> many atom,
        try nat, -- Sugar 8: Expand numerical to S...Z
        exprAux
      ]

exprAux :: Parser Expr
exprAux =
  trace "Parsing expr auxiliary"
    $ label
      "expression auxiliary"
    $ choice
      [ try $ do
          _ <- symbol "mu"
          branches <- brackets $ sepBy1 patternCase (symbol "|")
          return $ Mu branches,
        -- Sugar 7: Simplify mu[] as []
        try $ do
          branches <- brackets $ sepBy1 patternCase (symbol "|")
          return $ Mu branches,
        try nat,
        try $ Var <$> varId,
        parens expr
      ]

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
  trace "Parsing command"
    $ label
      "command"
    $ choice
      [ try commandSugar,
        try doThenCommand,
        try $ angles $ do
          e <- expr
          _ <- symbol "|>"
          Command e <$> expr
      ]

-- z = e;
-- x = e
-- ~y = co
decl :: Parser Decl
decl =
  trace "Parsing declaration"
    $ label
      "Declaration"
    $ choice
      [ try defDecl,
        try runDecl,
        do
          ident <- varId
          _ <- symbol "="
          Decl ident <$> expr
      ]

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
program = trace "Parsing program" $ label "program" $ Program <$> (sc *> decls <* eof)

-- Utility function to run the parser

-- Parse a string into a Command, consuming leading whitespace, this is used for testing
parseString :: String -> Either (ParseErrorBundle String Void) Command
parseString = parse (sc *> command <* eof) ""

-- Parse a file into a Program, consuming leading whitespace
parseFile :: String -> IO (Either (ParseErrorBundle String Void) Program)
parseFile file = do
  contents <- readFile file
  return $ parse (sc *> program <* eof) file contents

---- -- Sugar functions for parsing commands and declarations

-- | . and @ operator for generating commands
commandSugar :: Parser Command
commandSugar = trace "Parsing Sugared Command" $ do
  choice
    [ try $ do
        -- . operator sugar: x . y === < x |> y >
        e <- expr
        _ <- symbol "."
        Command e <$> expr,
      do
        -- @ operator sugar: x k @ fun === < Ap x k |> fun >
        choice
          [ try $ do
              fun <- expr
              _ <- symbol "@"
              args <- many expr
              return $ desugarAt fun args,
            do
              args <- many expr
              _ <- symbol "@"
              coDesugarAt args <$> expr
          ]
    ]
  where
    -- Desugar @ operator, when function is placed as expression
    desugarAt :: Expr -> [Expr] -> Command
    desugarAt _ [] = error "@ requires at least one argument"
    desugarAt fun args = Command (Cons "Ap" args) fun
    -- Desugar @ operator, when function is placed as co-expression
    coDesugarAt :: [Expr] -> Expr -> Command
    coDesugarAt [] _ = error "@ requires at least one argument"
    coDesugarAt args fun = Command (Cons "Ap" args) fun

-- | Sugar for definitions: def/run
-- def NAME ARGS* := COMMAND === NAME = mu[ Ap ARGS... -> COMMAND ]
defDecl :: Parser Decl
defDecl = trace "Parsing Sugared Declaration" $
  label "Sugared Declaration" $
    do
      _ <- symbol "def"
      name <- varId
      args <- many varId
      _ <- symbol ":="
      Decl name . desugarDef args <$> command
  where
    desugarDef :: [VarId] -> Command -> Expr
    desugarDef args cmd =
      Mu [(ConsPattern "Ap" args, cmd)]

-- | run COMMAND === main = mu[ Halt -> COMMAND ]
runDecl :: Parser Decl
runDecl = do
  _ <- symbol "run"
  Decl "main" . desugarRun <$> command
  where
    desugarRun :: Command -> Expr
    desugarRun cmd = Mu [(ConsPattern "Halt" [], cmd)]

-- Sugar 4: let grammar
letExpr :: Parser Expr
letExpr = do
  _ <- symbol "let"
  bindings <- sepBy1 binding (symbol ".")
  _ <- symbol "in"
  desugarLet bindings <$> expr
  where
    binding = do
      name <- varId
      _ <- symbol "="
      choice
        [ do e <- expr; return (name, Left e),
          do c <- command; return (name, Right c)
        ]
    desugarLet :: [(VarId, Either Expr Command)] -> Expr -> Expr
    desugarLet [] body = body
    desugarLet ((name, Left e) : rest) body =
      -- Simple substitution for now - proper implementation would need alpha conversion
      desugarLet rest body
    desugarLet ((name, Right cmd) : rest) body =
      desugarLet rest body

whereClause :: Parser [(VarId, Either Expr Command)]
whereClause = do
  _ <- symbol "where"
  sepBy1 binding (symbol ".")
  where
    binding = do
      name <- varId
      _ <- symbol "="
      choice
        [ do e <- expr; return (name, Left e),
          do c <- command; return (name, Right c)
        ]

-- Sugar: do...then grammar
doThenCommand :: Parser Command
doThenCommand = trace "do...then" $ label "do...then" $ do
  _ <- symbol "do"
  bindings <- many (try $ notFollowedBy (symbol "then") *> doBinding)
  _ <- symbol "then"
  desugarDoThen bindings <$> command
  where
    doBinding = do
      name <- varId
      _ <- symbol "<-"
      fun <- expr
      args <- many expr
      _ <- symbol ","
      return (name, fun, args)

    desugarDoThen :: [(VarId, Expr, [Expr])] -> Command -> Command
    desugarDoThen [] cmd = cmd
    desugarDoThen ((name, fun, args) : rest) cmd =
      -- This creates nested applications with continuations
      Command fun (Cons "Ap" (args ++ [Mu [(VarPattern name, desugarDoThen rest cmd)]]))
