{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser
  ( parseMiniMu,
    parseSugaredMiniMu,
    parseFile,
    sugarExpr,
    sugarCommand,
    sugarDecl )
where

import Data.Void
import Syntax
import Sugar (desugarProgram)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Fresh (fresh)

type Parser = Parsec Void String

-- Keywords for our language
keywords :: [String]
keywords =
  [ "fn",
    "run",
    "let",
    "letc",
    "in",
    "where",
    "seq",
    "do",
    "then",
    "match",
    "patch",
    "with",
    "have",
    "here"
  ]

-- | Parse a MiniMu program from a file, handing errors, main entry point for parsing | --
parseSugaredMiniMu :: FilePath -> IO SugarProgram
parseSugaredMiniMu file = do
  ast <- parseFile file
  either
    ( \err -> do
        putStrLn $ errorBundlePretty err
        error "Failed to parse MiniMu program"
    )
    return
    ast

parseMiniMu :: FilePath -> IO Program
parseMiniMu file = do
  ast <- parseFile file
  either
    ( \err -> do
        putStrLn $ errorBundlePretty err
        error "Failed to parse MiniMu program"
    )
    (return . fresh . desugarProgram)
    ast

-- | Parse a file into a Program, consuming leading whitespace | --
parseFile :: String -> IO (Either (ParseErrorBundle String Void) SugarProgram)
parseFile file = do
  contents <- readFile file
  return $ parse (sc *> sugarProgram <* eof) file contents

-- | Space, comments consumer | --
sc :: Parser ()
sc =
  L.space
    space1 -- Consume space, newline, tab
    (L.skipLineComment "--") -- Skip line comments starting with "--"
    (L.skipBlockComment "{-" "-}") -- Skip block comments between "{-" and "-}"

-- | Helper for lexemes: consumes trailing whitespace | --
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a specific symbol and consume trailing whitespace | --
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse something between curly braces | --
curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

-- | Parse something between angle brackets | --
angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

-- | Parse something between square brackets | --
squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

-- | Parse something between parentheses | --
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Utilities definitions for ids | --

-- | Parse file identifier (a string in quotes) | --
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

-- | Parse a constructor (starting with Uppercase) | --
consIdentifier :: Parser String
consIdentifier =
  label
    "constructor name"
    $ lexeme
    $ do
      first <- upperChar
      rest <- many (alphaNumChar <|> char '_' <|> char ':' <|> char '\'')
      return (first : rest)

-- | Parse variable name (starting with lowercase) | --
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

-- | Parse command identifiers (starting with lowercase) | --
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

-- | Parse variable ids, command ids, and constructor ids | --

-- | Parse variable ids | --
varId :: Parser VarId
varId =
  label "variable id" $ lexeme varIdentifier

-- | Parse command ids | --
commandId :: Parser CommandId
commandId =
  label "command id" $ lexeme cmdIdentifier

-- same for constructors
consId :: Parser ConsId
consId =
  label "constructor id" $ lexeme consIdentifier

-- | Parse a full MiniMu sugar program | --
sugarProgram :: Parser SugarProgram
sugarProgram = label "sugar program" $ do
  imports <- many importDecl
  decls <- sugarDecls
  SugarProgram 
    imports 
    decls 
    <$> option [] exportList

-- | Parse import declarations | --
importDecl :: Parser ImportDecl
importDecl = do
  _ <- symbol "import"
  moduleName <- fileIdentifier -- import "module" a b c
  vars <- parens $ sepBy1 varId (symbol ",")
  return $ ImportDecl moduleName vars

-- | Parse export list | --
exportList :: Parser [VarId]
exportList = do
  _ <- symbol "export"
  sepBy1 varId (symbol ",")


-- | Utility Parsers | --

-- | Parse pattern case for sugared commands | --
sugarBranch :: Parser (Pattern, SugarCommand)
sugarBranch =
  label "sugar pattern case" $
    (,) <$> pattern <* symbol "->" <*> sugarCommand


-- | Sugared Syntax Parsers | --

-- | Parse multiple sugared declarations | --
sugarDecls :: Parser [SugarDecl]
sugarDecls = many $ notFollowedBy (symbol "export") *> sugarDecl <* symbol ";"

-- | Parse SugarDecl | --

-- | Parse sugared function declaration | --
sugarFuncDecl :: Parser SugarDecl
sugarFuncDecl = label "sugar func decl" $ do
  _ <- symbol "fn"
  f <- varId
  args <- many varId
  _ <- symbol ":="
  FuncDecl f args <$> sugarCommand

-- | Parse sugared default declaration | --
sugarDefaultDecl :: Parser SugarDecl
sugarDefaultDecl = label "sugar default decl" $ do
  name <- varId
  _ <- symbol ":="
  DefaultDecl name <$> sugarExpr

-- | Parse run declaration (run M => main = { halt -> M }) | --
sugarRunDecl :: Parser SugarDecl
sugarRunDecl = label "run declaration" $ do
  _ <- symbol "run"
  RunDecl <$> sugarCommand

-- | Main sugared declaration parser | --
sugarDecl :: Parser SugarDecl
sugarDecl =
  label "sugar declaration" $
    choice
      [ try sugarRunDecl,
        try sugarFuncDecl,
        sugarDefaultDecl
      ]


-- | Parse SugarCommand | --

-- | Parse sugared let command | --
sugarLetCommand :: Parser SugarCommand
sugarLetCommand = label "sugar let command" $ do
  _ <- symbol "let"
  var <- varId
  _ <- symbol "="
  e <- sugarExpr
  _ <- symbol "in"
  LetCommand var e <$> sugarCommand

-- | Parse sugared letc command | --
sugarLetcCommand :: Parser SugarCommand
sugarLetcCommand = label "sugar letc command" $ do
  _ <- symbol "letc"
  var <- varId
  _ <- symbol "="
  e <- sugarExpr
  _ <- symbol "in"
  LetcCommand var e <$> sugarCommand

-- | Parse sugared match command | --
sugarMatchCommand :: Parser SugarCommand
sugarMatchCommand = label "sugar match command" $ do
  _ <- symbol "match"
  e <- sugarExpr
  _ <- symbol "with"
  _ <- optional (symbol "|")
  cases <- sepBy1 sugarBranch (symbol "|")
  return $ MatchCommand e cases

-- | Parse sugared patch command | --
sugarPatchCommand :: Parser SugarCommand
sugarPatchCommand = label "sugar patch command" $ do
  _ <- symbol "patch"
  e <- sugarExpr
  _ <- symbol "with"
  _ <- optional (symbol "|")
  cases <- sepBy1 sugarBranch (symbol "|")
  return $ PatchCommand e cases

-- | Parse do/then binding | --
doThenBinding :: Parser DoThenBinding
doThenBinding = label "do/then binding" $ do
  pat <- pattern
  _ <- symbol "<-"
  Binding pat <$> sugarExpr

-- | Parse sugared do/then command | --
sugarDoThenCommand :: Parser SugarCommand
sugarDoThenCommand = label "sugar do/then command" $ do
  _ <- symbol "do"
  bindings <- sepBy (notFollowedBy (symbol "then") *> doThenBinding) (symbol ",")
  _ <- symbol "then"
  DoThenCommand bindings <$> sugarCommand

-- | Parse sugared @ command | --
sugarAtCommand :: Parser SugarCommand
sugarAtCommand = label "sugar @ command" $ do
  choice
    [ try $ do
        -- f @ a b c => f . (a, b, c)
        fun <- sugarExpr
        _ <- symbol "@"
        args <- many sugarAtom
        return $ AtCommand fun args,
      do
        -- a b c @ f => (a, b, c) . f
        args <- many sugarAtom
        _ <- symbol "@"
        CoAtCommand args <$> sugarExpr
    ]

-- | Parse sugared . command | --
sugarDotCommand :: Parser SugarCommand
sugarDotCommand = label "sugar . command" $ do
  e1 <- sugarExpr
  _ <- symbol "."
  DotCommand e1 <$> sugarExpr

-- | Main sugared command parser | --
sugarCommand :: Parser SugarCommand
sugarCommand =
  label "sugar command" $
    choice
      [ try sugarLetCommand,
        try sugarLetcCommand,
        try sugarMatchCommand,
        try sugarPatchCommand,
        try sugarDoThenCommand,
        try sugarAtCommand,
        try sugarDotCommand,
        SugarCommandVar <$> commandId
      ]


-- | Parse SugarExpr | --

-- Parse delimited expression < ... >
-- delimExpr :: Parser SugarExpr
-- delimExpr = label "delimited expression" $ do
--   _ <- symbol "<"
--   cmd <- sugarCommand
--   _ <- symbol ">"
--   return $ DelimExpr cmd

-- | Parse have bindings | --
sugarHaveBinding :: Parser HaveBinding
sugarHaveBinding = label "have binding" $ do
  choice
    [ try $ do
        _ <- symbol "'"
        cmdId <- commandId
        _ <- symbol "="
        HaveCommandBinding cmdId <$> sugarCommand,
      do
        var <- varId
        _ <- symbol "="
        HaveExprBinding var <$> sugarExpr
    ]

-- | Parse have expression | --
sugarHaveExpr :: Parser SugarExpr
sugarHaveExpr = label "have expression" $ do
  _ <- symbol "have"
  bindings <- sepBy1 sugarHaveBinding (symbol ",")
  _ <- symbol "in"
  HaveExpr bindings <$> sugarExpr

-- | Parse sugared constructor application | --
sugarCons :: Parser SugarExpr
sugarCons =
  label "sugar constructor" $
    SugarCons <$> consId <*> many sugarAtom

-- | Parse sugared mu expression | --
sugarMu :: Parser SugarExpr
sugarMu = label "sugar mu" $ do
  _ <- symbol "{"
  cases <- sepBy1 sugarBranch (symbol "|")
  _ <- symbol "}"
  return $ SugarMu cases

-- | Parse sugared atoms | --
sugarAtom :: Parser SugarExpr
sugarAtom =
  label "sugar atom" $
    choice
      [ try sugarMu,
        try sugarHaveExpr,
        try sugarNatLit,
        try sugarTupleLit,
        try sugarListLit,
        -- try delimExpr,
        try $ SugarCons <$> consId <*> pure [],
        try $ SugarVar <$> varId,
        parens sugarExpr
      ]

-- | Main sugared expression parser | --
sugarExpr :: Parser SugarExpr
sugarExpr =
  label "sugar expression" $
    choice
      [ try sugarCons,
        try sugarAppExpr,
        try sugarCoAppExpr,
        sugarAtom
      ]

-- | Parse sugared function application: f{k1, k2}(x1, x2, k1, k2) | --
sugarAppExpr :: Parser SugarExpr
sugarAppExpr = label "sugar app expression" $ do
  fun <- sugarAtom
  explicitConts <- option [] (curly (sepBy1 sugarExpr (symbol ",")))
  _ <- symbol "("
  args <- sepBy sugarExpr (symbol ",")
  _ <- symbol ")"
  return $ AppExpr fun explicitConts args

-- | Parse sugared cofunction application: 'f{k1, k2}(x1, x2, k1, k2) | --
sugarCoAppExpr :: Parser SugarExpr
sugarCoAppExpr = label "sugar coapp expression" $ do
  _ <- symbol "'"
  cmdId <- commandId
  explicitConts <- option [] (curly (sepBy1 sugarExpr (symbol ",")))
  _ <- symbol "("
  args <- sepBy sugarExpr (symbol ",")
  _ <- symbol ")"
  return $ CoAppExpr cmdId explicitConts args

-- | Parse natural number as SugarExpr | --
sugarNatLit :: Parser SugarExpr
sugarNatLit = label "natural number literal" $ NatLit <$> lexeme L.decimal

-- | Parse sugared tuple literals, at least 2 elems | --
sugarTupleLit :: Parser SugarExpr
sugarTupleLit = label "tuple literal" $ do
  _ <- symbol "("
  first <- sugarExpr
  _ <- symbol ","
  rest <- sepBy1 sugarExpr (symbol ",")
  _ <- symbol ")"
  return $ TupLit (first : rest)

-- | Parse sugared list literals | --
sugarListLit :: Parser SugarExpr
sugarListLit = label "list literal" $ do
  _ <- symbol "["
  elems <- sepBy sugarExpr (symbol ",")
  _ <- symbol "]"
  return $ ListLit elems

-- | Parse Patterns | --

-- | Parse tuple patterns with arbitrary length (minimum 2 elements)  
tuplePattern :: Parser Pattern
tuplePattern = label "tuple pattern" $ do
  _ <- symbol "("
  first <- pattern
  _ <- symbol ","
  rest <- sepBy1 pattern (symbol ",")
  _ <- symbol ")"
  return $ ConsPattern "Tuple" (first : rest)

-- | Parse natural number patterns | --
natPattern :: Parser Pattern
natPattern = label "natural number pattern" $ intToPeanoPattern <$> lexeme L.decimal

-- | Convert integer to Peano pattern | --
intToPeanoPattern :: Integer -> Pattern
intToPeanoPattern 0 = ConsPattern "Z" []
intToPeanoPattern n = ConsPattern "S" [intToPeanoPattern (n - 1)]

-- | Parse a pattern | --
pattern :: Parser Pattern
pattern =
  label "pattern" $
    choice
      [ try $ WildcardPattern <$ symbol "_", -- wild card pattern (e.g., "_")
        try natPattern, -- Sugar 6: Expand numerical patterns to S...Z
        try tuplePattern, -- Sugar 9: Expand pairs
        try $ ConsPattern <$> consId <*> many pattern, -- e.g., "C x _ z"
        try $ VarPattern <$> varId, -- variable patterns (e.g., "x")
        parens pattern -- Allow parentheses for grouping
      ]
