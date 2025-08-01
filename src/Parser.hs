module Parser
  ( 
    parseMiniMu,
    parseFile,
  )
where

import Data.Void
import Debug.Trace
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Keywords for our language
keywords :: [String]
keywords =
  [ "mu",
    "def",
    "run",
    "let",
    "in",
    "where",
    "do",
    "then"
  ]

-- Parse a MiniMu program from a file, handing errors
-- Main entry point for parsing
parseMiniMu :: FilePath -> IO Program
parseMiniMu file = do
  ast <- parseFile file
  either (\err -> do
    putStrLn $ errorBundlePretty err
    error "Failed to parse MiniMu program")
    return ast

-- Parse a file into a Program, consuming leading whitespace
parseFile :: String -> IO (Either (ParseErrorBundle String Void) Program)
parseFile file = do
  contents <- readFile file
  return $ parse (sc *> program <* eof) file contents

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

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

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

fileIdentifier :: Parser String
fileIdentifier =
  label
    "file identifier"
    $ lexeme
    $ do
      _ <- symbol "\""
      first <- lowerChar
      rest <- many (alphaNumChar <|> char '_' <|> char '-' <|> char '.')
      _ <- symbol "\""
      return (first : rest)

cmdIdentifier :: Parser String
cmdIdentifier =
  label
    "command identifier"
    $ lexeme
    $ do
      first <- lowerChar
      rest <- many (alphaNumChar <|> char '_' <|> char ':' <|> char '\'')
      let word = first : rest
      if word `elem` keywords
        then fail $ "keyword " ++ show word ++ " cannot be the name of a command"
        else return word

nat :: Parser Expr
nat = label "natural number" $ lexeme $ do
  intToPeano <$> L.decimal
  where
    intToPeano :: Integer -> Expr
    intToPeano 0 = Cons "Z" []
    intToPeano n = Cons "S" [intToPeano (n - 1)]

pair :: Parser Expr
pair = label "pair" $ lexeme $ do
  _ <- symbol "("
  x <- expr
  _ <- symbol ","
  y <- expr   
  _ <- symbol ")"
  return $ Cons "Pair" [x, y]

natPattern :: Parser Pattern
natPattern = label "natural number pattern" $ lexeme $ do
  intToPeanoPattern <$> L.decimal
  where
    intToPeanoPattern :: Integer -> Pattern
    intToPeanoPattern 0 = ConsPattern "Z" []
    intToPeanoPattern n = ConsPattern "S" [intToPeanoPattern (n - 1)]

pairPattern :: Parser Pattern  
pairPattern = label "pair pattern" $ do
  _ <- symbol "("
  x <- patr
  _ <- symbol ","
  y <- patr
  _ <- symbol ")"
  return $ ConsPattern "Pair" [x, y]

-- varID cannot start with ~
varId :: Parser VarId
varId =
  label
    "variable id"
    $ lexeme
    $ do
      -- notFollowedBy (char '~')
      varIdentifier

commandId :: Parser CommandId
commandId =
  label
    "command id"
    $ lexeme
    $ do
      -- notFollowedBy (char '~')
      cmdIdentifier

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
patr :: Parser Pattern
patr = 
  label "pattern" $ choice
    [ try wildcardPattern,
      try natPattern,  -- Sugar 6: Expand numerical patterns to S...Z
      try pairPattern, -- Sugar 9: Expand pairs
      try consPattern, 
      try varPattern,
      parens patr  -- Allow parentheses for grouping
    ]

wildcardPattern :: Parser Pattern
wildcardPattern = do
  _ <- symbol "_"
  return WildcardPattern

consPattern :: Parser Pattern
consPattern = do
  c <- consId
  args <- many patr  -- Now parses nested patterns
  return $ ConsPattern c args

varPattern :: Parser Pattern  
varPattern = VarPattern <$> varId

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
      pat <- patr
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
        try pair,
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
          branches <- curly $ sepBy1 patternCase (symbol "|")
          return $ Mu branches,
        try nat, -- Sugar 8: Expand numerical to S...Z
        try pair, -- Sugar 9: Expand pairs
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
      [ try letCommand, 
        try commandSugar,
        try doThenCommand,
        try $ angles $ do
          e <- expr
          _ <- symbol "|>"
          Command e <$> expr,
        CommandVar <$> commandId
      ]

-- z = e;
-- x = e
-- ~y = co
-- Parse export declaration
importDecl :: Parser ImportDecl
importDecl = do
  _ <- symbol "import"
  moduleName <- fileIdentifier -- import "module" a b c
  vars <- parens $ sepBy1 varId (symbol ",")
  return $ ImportDecl moduleName vars

-- Parse export list
exportList :: Parser [VarId]
exportList = do
  _ <- symbol "export"
  sepBy1 varId (symbol ",")

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
program = do
  sc
  imports <- many importDecl      -- imports at beginning
  _decls <- decls  -- declarations in middle
  exports <- option [] exportList    -- optional exports at end
  eof
  return $ Program imports _decls exports

-- Utility function to run the parser

-- Parse a string into a Command, consuming leading whitespace, this is used for testing
parseString :: String -> Either (ParseErrorBundle String Void) Command
parseString = parse (sc *> command <* eof) ""

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
      args <- many patr
      _ <- symbol ":="
      Decl name . desugarDef args <$> command
  where
    desugarDef :: [Pattern] -> Command -> Expr
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
letExpr = label "let expression" $ trace "Parsing let expression" $ do
  _ <- symbol "let"
  bindings <- sepBy1 binding (symbol ",")
  _ <- symbol "in"
  desugarLet bindings <$> expr
  where
    desugarLet :: [(Either VarId CommandId, Either Expr Command)] -> Expr -> Expr
    desugarLet [] body = body
    desugarLet bindings body =
      foldr
        ( \(name, value) acc -> case name of
            Left varid -> case value of
              Left e -> findAndSubstExprInExpr varid e acc
              Right _ -> error "varId cannot be bound to a command"
            Right cmdid -> case value of
              Left _ -> error "commandId cannot be bound to an expression"
              Right c -> findAndSubstCmdInExpr cmdid c acc
        )
        body
        bindings

letCommand :: Parser Command
letCommand = trace "Parsing let command" $ label "let command" $ do
  _ <- symbol "let"
  bindings <- sepBy1 binding (symbol ",")
  _ <- symbol "in"
  desugarLetCommand bindings <$> command
  where
    desugarLetCommand :: [(Either VarId CommandId, Either Expr Command)] -> Command -> Command
    desugarLetCommand [] cmd = cmd
    desugarLetCommand bindings cmd =
      foldr
        ( \(name, value) acc -> case name of
            Left varid -> case value of
              Left e -> findAndSubstExprInCmd varid e acc
              Right _ -> error "varId cannot be bound to a command"
            Right cmdid -> case value of
              Left _ -> error "commandId cannot be bound to an expression"
              Right c -> findAndSubstCmdInCmd cmdid c acc
        )
        cmd
        bindings

binding :: Parser (Either VarId CommandId, Either Expr Command)
binding = trace "Parsing binding in let/where" $ do
      choice
        [  try $ trace "Parsing a command binding" $ do
            name <- commandId
            _ <- symbol "="
            c <- command
            return (Right name, Right c)
        , trace "Parsing an expr binding" $ do
            name <- varId
            _ <- symbol "="
            e <- expr
            return (Left name, Left e)
        ]

findAndSubstExprInExpr :: VarId -> Expr -> Expr -> Expr
findAndSubstExprInExpr name e (Mu branches) =
  Mu (map (\(p, c) -> (p, findAndSubstExprInCmd name e c)) branches)
findAndSubstExprInExpr name e (Cons c args) =
  Cons c (map (findAndSubstExprInExpr name e) args)
findAndSubstExprInExpr name e (Var v) =
  if v == name then e else Var v

findAndSubstExprInCmd :: VarId -> Expr -> Command -> Command
findAndSubstExprInCmd name e (Command expr1 expr2) =
  Command (findAndSubstExprInExpr name e expr1) (findAndSubstExprInExpr name e expr2)
findAndSubstExprInCmd _ _ (CommandVar cmdId) =
  CommandVar cmdId -- No substitution for command variables

findAndSubstCmdInExpr :: CommandId -> Command -> Expr -> Expr
findAndSubstCmdInExpr name cmd (Mu branches) =
  Mu (map (\(p, c) -> (p, findAndSubstCmdInCmd name cmd c)) branches)
findAndSubstCmdInExpr name cmd (Cons c args) =
  Cons c (map (findAndSubstCmdInExpr name cmd) args)
findAndSubstCmdInExpr _ _ (Var v) =
  Var v

findAndSubstCmdInCmd :: CommandId -> Command -> Command -> Command
findAndSubstCmdInCmd name cmd (Command expr1 expr2) =
  Command (findAndSubstCmdInExpr name cmd expr1) (findAndSubstCmdInExpr name cmd expr2)
findAndSubstCmdInCmd name cmd (CommandVar cmdId) =
  if cmdId == name then cmd else CommandVar cmdId

-- whereClause :: Parser [(VarId, Either Expr Command)]
-- whereClause = do
--   _ <- symbol "where"
--   sepBy1 binding (symbol ".")
--   where
--     binding = do
--       name <- varId
--       _ <- symbol "="
--       choice
--         [ do e <- expr; return (name, Left e),
--           do c <- command; return (name, Right c)
--         ]

-- Sugar: do...then grammar
doThenCommand :: Parser Command
doThenCommand = trace "Parsing do/then command" $ label "do/then command" $ do
  _ <- symbol "do"
  bindings <- many (try $ notFollowedBy (symbol "then") *> doBinding)
  _ <- symbol "then"
  desugarDoThen bindings <$> command
  where
    doBinding = do
      pat <- patr
      _ <- symbol "<-"
      fun <- expr
      args <- many expr
      _ <- symbol ","
      return (pat, fun, args)

    desugarDoThen :: [(Pattern, Expr, [Expr])] -> Command -> Command
    desugarDoThen [] cmd = cmd
    desugarDoThen ((pat, fun, args) : rest) cmd =
      -- This creates nested applications with continuations
      Command fun (Cons "Ap" (args ++ [Mu [(pat, desugarDoThen rest cmd)]]))
