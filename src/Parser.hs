module Parser
  ( parseMiniMu,
    parseFile,
  )
where

import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Keywords for our language
keywords :: [String]
keywords =
  [ "def",
    "run",
    "let",
    "in",
    "where",
    "seq",
    "then"
  ]

-- Parse a MiniMu program from a file, handing errors
-- Main entry point for parsing
parseMiniMu :: FilePath -> IO Program
parseMiniMu file = do
  ast <- parseFile file
  either
    ( \err -> do
        putStrLn $ errorBundlePretty err
        error "Failed to parse MiniMu program"
    )
    return
    ast

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
    $ do
      first <- lowerChar
      rest <- many (alphaNumChar <|> char '_' <|> char ':' <|> char '\'')
      let word = first : rest
      if word `elem` keywords
        then fail $ "keyword " ++ show word ++ " cannot be the name of a command"
        else return word

pair :: String -> (a -> a -> a) -> Parser a -> Parser a
pair name f p = label name $ f <$ symbol "(" <*> p <* symbol "," <*> p <* symbol ")"

pairExpr :: Parser Expr
pairExpr = pair "pair" (\x y -> Cons "Pair" [x, y]) expr

pairPattern :: Parser Pattern
pairPattern = pair "pair pattern" (\x y -> ConsPattern "Pair" [x, y]) pattern

natExpr :: Parser Expr
natExpr = label "natural number" $ intToPeano <$> lexeme L.decimal

natPattern :: Parser Pattern
natPattern = label "natural number pattern" $ intToPeanoPattern <$> lexeme L.decimal

intToPeano :: Integer -> Expr
intToPeano 0 = Cons "Z" []
intToPeano n = Cons "S" [intToPeano (n - 1)]

intToPeanoPattern :: Integer -> Pattern
intToPeanoPattern 0 = ConsPattern "Z" []
intToPeanoPattern n = ConsPattern "S" [intToPeanoPattern (n - 1)]

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
pattern :: Parser Pattern -- TODO: rename to pattern
pattern =
  label "pattern" $
    choice
      [ try $ WildcardPattern <$ symbol "_", -- wild card pattern (e.g., "_")
        try natPattern, -- Sugar 6: Expand numerical patterns to S...Z
        try pairPattern, -- Sugar 9: Expand pairs
        try $ ConsPattern <$> consId <*> many pattern, -- e.g., "C x _ z"
        try $ VarPattern <$> varId, -- variable patterns (e.g., "x")
        parens pattern -- Allow parentheses for grouping
      ]

-- Parse a case pattern -> command in a Mu
patternCase :: Parser (Pattern, Command)
patternCase = label "pattern case" $ (,) <$> pattern <* symbol "->" <*> command

-- atoms are naturally delimited expressions.
atom :: Parser Expr
atom =
  label "atom expr" $
    choice
      [ -- Sugar 7: Simplify mu[] as []
        try (Mu <$> curly (sepBy1 patternCase (symbol "|"))),
        try letExpr, -- TODO: move to atom so we allow: x @ X let y = ... in ...
        try natExpr, -- Sugar 8: Expand numerical to S...Z
        try pairExpr, -- Sugar 9: Expand pairs
        try $ (`Cons` []) <$> consId, -- constructor with no arguments
        try $ Var <$> varId,
        parens expr
      ]

expr :: Parser Expr
expr =
  label
    "expression"
    $ choice
      [ try completeCons,
        -- notFollowedBy (symbol "_") will trigger backtracking
        try incompleteCons,
        atom
      ]

completeCons :: Parser Expr
completeCons =
  label "complete constructor" $
    Cons <$> consId <*> many atom <* notFollowedBy (symbol "_")

incompleteCons :: Parser Expr
incompleteCons =
  label "incomplete constructor" $
    IncompleteCons
      <$> consId
      <*> many (try (Left <$> atom) <|> (Right <$> consHole))

consHole :: Parser HoleExpr
consHole =
  label "constructor hole" $
    HoleExpr <$ symbol "_"

-- Parse a command
command :: Parser Command
command =
  label
    "command"
    $ choice
      [ try letCommand,
        try commandSugar,
        try seqThenCommand,
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
  label
    "Declaration"
    $ choice
      [ try defDecl,
        try runDecl,
        Decl <$> varId <* symbol "=" <*> expr
      ]

decls :: Parser [Decl]
decls = many $ notFollowedBy (symbol "export") *> decl <* symbol ";"

-- (do
-- d <- decl
-- semi
-- ds <- decls
-- return (d : ds))
-- <|> (return [])

program :: Parser Program
program = do
  sc
  imports <- many importDecl -- imports at beginning
  _decls <- decls -- declarations in middle
  exports <- option [] exportList -- optional exports at end
  eof
  return $ Program imports _decls exports

-- Utility function to run the parser

---- -- Sugar functions for parsing commands and declarations

-- | . and @ operator for generating commands
commandSugar :: Parser Command
commandSugar = label "Sugared Command" $ do
  choice
    [ try $ Command <$> expr <* symbol "." <*> expr,
      -- . operator sugar: x . y === < x |> y >
      do
        -- @ operator sugar: x k @ fun === < Ap x k |> fun >
        -- X Y Z @ U V W
        -- x @ Y V W
        -- {} @ U V W
        -- let x = .. in (... @ ...)
        -- ... @ let x = .. in ...
        -- x y z @ y
        choice
          [ try $ desugarAt <$> atom <* symbol "@" <*> many atom, -- fun @ a b c
            coDesugarAt <$> many atom <* symbol "@" <*> atom -- a b c @ fun
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
defDecl =
  label "Sugared Declaration" $
    (\n args cmd -> Decl n (desugarDef args cmd))
      <$> (symbol "def" *> varId)
      <*> many pattern
      <* symbol ":="
      <*> command
  where
    desugarDef :: [Pattern] -> Command -> Expr
    desugarDef args cmd =
      Mu [(ConsPattern "Ap" args, cmd)]

-- | run COMMAND === main = mu[ halt -> COMMAND ]
runDecl :: Parser Decl
runDecl = do
  Decl "main" . desugarRun <$> (symbol "run" *> command)
  where
    desugarRun :: Command -> Expr
    desugarRun cmd = Mu [(VarPattern "halt", cmd)]

-- Sugar 4: let grammar
letExpr :: Parser Expr
letExpr = label "let expression" $ do
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
              Left e -> findAndSubstExprInExpr (varid, e) acc
              Right _ -> error "varId cannot be bound to a command"
            Right cmdid -> case value of
              Left _ -> error "commandId cannot be bound to an expression"
              Right c -> findAndSubstCmdInExpr (cmdid, c) acc
        )
        body
        bindings

letCommand :: Parser Command
letCommand = label "let command" $ do
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
              Left e -> findAndSubstExprInCmd (varid, e) acc
              Right _ -> error "varId cannot be bound to a command"
            Right cmdid -> case value of
              Left _ -> error "commandId cannot be bound to an expression"
              Right c -> findAndSubstCmdInCmd (cmdid, c) acc
        )
        cmd
        bindings

binding :: Parser (Either VarId CommandId, Either Expr Command)
binding = label "Parsing binding in let/where" $ do
  choice
    [ try $ label "Parsing a command binding" $ do
        name <- commandId
        _ <- symbol "="
        c <- command
        return (Right name, Right c),
      label "Parsing an expr binding" $ do
        name <- varId
        _ <- symbol "="
        e <- expr
        return (Left name, Left e)
    ]

-- Helper functions for substitution in let/where desugaring
type ExprBinding = (VarId, Expr)

type CommandBinding = (CommandId, Command)

-- Structural recursions on Expr and Command for substitution

findAndSubstExprInExpr :: ExprBinding -> Expr -> Expr
findAndSubstExprInExpr (name, e) (Mu branches) =
  Mu (map (\(p, c) -> (p, findAndSubstExprInCmd (name, e) c)) branches)
findAndSubstExprInExpr (name, e) (Cons c args) =
  Cons c (map (findAndSubstExprInExpr (name, e)) args)
findAndSubstExprInExpr (name, e) (IncompleteCons c args) =
  IncompleteCons c (map (either (Left . findAndSubstExprInExpr (name, e)) Right) args)
findAndSubstExprInExpr (name, e) (Var v) =
  if v == name then e else Var v

findAndSubstExprInCmd :: ExprBinding -> Command -> Command
findAndSubstExprInCmd (name, e) (Command expr1 expr2) =
  Command (findAndSubstExprInExpr (name, e) expr1) (findAndSubstExprInExpr (name, e) expr2)
findAndSubstExprInCmd _ (CommandVar cmdId) =
  CommandVar cmdId -- No substitution for command variables

findAndSubstCmdInExpr :: CommandBinding -> Expr -> Expr
findAndSubstCmdInExpr (name, cmd) (Mu branches) =
  Mu (map (\(p, c) -> (p, findAndSubstCmdInCmd (name, cmd) c)) branches)
findAndSubstCmdInExpr (name, cmd) (Cons c args) =
  Cons c (map (findAndSubstCmdInExpr (name, cmd)) args)
findAndSubstCmdInExpr (name, cmd) (IncompleteCons c args) =
  IncompleteCons c (map (either (Left . findAndSubstCmdInExpr (name, cmd)) Right) args)
findAndSubstCmdInExpr _ (Var v) =
  Var v

findAndSubstCmdInCmd :: CommandBinding -> Command -> Command
findAndSubstCmdInCmd (name, cmd) (Command expr1 expr2) =
  Command (findAndSubstCmdInExpr (name, cmd) expr1) (findAndSubstCmdInExpr (name, cmd) expr2)
findAndSubstCmdInCmd (name, cmd) (CommandVar cmdId) =
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
seqThenCommand :: Parser Command
seqThenCommand = label "seq/then command" $ do
  _ <- symbol "seq"
  bindings <- sepBy (notFollowedBy (symbol "then") *> doBinding) (symbol ",")
  _ <- symbol "then"
  desugarDoThen bindings <$> command
  where
    doBinding = do
      pat <- pattern
      _ <- symbol "<-"
      fun <- expr
      args <- many expr
      return (pat, fun, args)

    desugarDoThen :: [(Pattern, Expr, [Expr])] -> Command -> Command
    desugarDoThen [] cmd = cmd
    desugarDoThen ((pat, fun, args) : rest) cmd =
      -- This creates nested applications with continuations
      Command fun (Cons "Ap" (args ++ [Mu [(pat, desugarDoThen rest cmd)]]))
