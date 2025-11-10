{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

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
  [ "fn",
    "run",
    "let",
    "letc",
    "in",
    "where",
    "do",
    "then",
    "match",
    "patch",
    "with",
    "here"
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

-- Parse something between square brackets
squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

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

-- Parse tuple expressions with arbitrary length (minimum 2 elements)
tupleExpr :: Parser Expr
tupleExpr = label "tuple expression" $ do
  _ <- symbol "("
  first <- expr
  _ <- symbol ","
  rest <- sepBy1 expr (symbol ",")
  _ <- symbol ")"
  return $ Cons "Tuple" (first : rest)

-- Parse tuple patterns with arbitrary length (minimum 2 elements)  
tuplePattern :: Parser Pattern
tuplePattern = label "tuple pattern" $ do
  _ <- symbol "("
  first <- pattern
  _ <- symbol ","
  rest <- sepBy1 pattern (symbol ",")
  _ <- symbol ")"
  return $ ConsPattern "Tuple" (first : rest)

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
        try tuplePattern, -- Sugar 9: Expand pairs
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
        try haveExpr, -- 
        try natExpr, -- Sugar 8: Expand numerical to S...Z
        try tupleExpr, -- Sugar 9: Expand pairs
        try $ (`Cons` []) <$> consId, -- constructor with no arguments
        try $ Var <$> varId,
        parens expr
      ]

-- Function application sugar: FUN(E1, E2, E3) => { _k -> FUN . { _f -> (E1, E2, E3, _k) } }
-- FUN{K1, K2}(E1, K1, E2, K2) => { (K1, K2) -> FUN . {_f -> (E1, K1, E2, K2) } }
funApplication :: Parser Expr
funApplication = label "function application" $ do
  fun <- atom
  -- Check for explicit continuations {K1, K2, ...}
  explicitConts <- option [] (curly (sepBy1 varId (symbol ",")))
  _ <- symbol "("
  args <- sepBy expr (symbol ",")
  _ <- symbol ")"

  if null explicitConts
    then do
      -- Simple case: FUN(E1, E2, E3) => { _k -> FUN . { _f -> (E1, E2, E3, _k) } }
      return $ Mu [(VarPattern "_k", Command fun (Mu [(VarPattern "_f", Command (Cons "Tuple" (args ++ [Var "_k"])) (Var "_f"))]))]
    else do
      -- With explicit conts: FUN{K1, K2}(E1, K1, E2, K2) => { (K1, K2) -> FUN . {_f -> (E1, K1, E2, K2) } }
      let contsPat = ConsPattern "Tuple" (map VarPattern explicitConts)
      let innerCmd = Command (Cons "Tuple" args) (Var "_f")
      return $ Mu [(contsPat, Command fun (Mu [(VarPattern "_f", innerCmd)]))]

-- Cofun application sugar: COFUN(E1, E2, E3) => { _k -> { _f -> (E1, E2, E3, _k) } . COFUN }
-- COFUN{K1, K2}(E1, K1, E2, K2) => { (K1, K2) -> {_f -> (E1, K1, E2, K2) } . COFUN }
cofunApplication :: Parser Expr
cofunApplication = label "cofunction application" $ do
  _ <- symbol "`"
  cofun <- atom
  explicitConts <- option [] (curly (sepBy1 varId (symbol ",")))
  _ <- symbol "("
  args <- sepBy expr (symbol ",")
  _ <- symbol ")"

  if null explicitConts
    then do
      -- simple case: `COFUN(E1, E2, E3) => { _k -> { _f -> (E1, E2, E3, _k) } . COFUN }
      let k = Var "_k"
      let innerMu = Mu [(VarPattern "_f", Command (Cons "Tuple" (args ++ [k])) (Var "_f"))]
      return $ Mu [(VarPattern "_k", Command innerMu cofun)]
    else do
      -- with explicit conts: `COFUN{K1, K2}(E1, K1, E2, K2) => { (K1, K2) -> {_f -> (E1, K1, E2, K2) } . COFUN }
      let contsPat = ConsPattern "Tuple" (map VarPattern explicitConts)
      let innerMu = Mu [(VarPattern "_f", Command (Cons "Tuple" args) (Var "_f"))]
      return $ Mu [(contsPat, Command innerMu cofun)]

expr :: Parser Expr
expr =
  label
    "expression"
    $ choice
      [ try funApplication,
        try cofunApplication,
        try cons,
        atom
      ]

cons :: Parser Expr
cons =
  label "complete constructor" $
    -- notFollowedBy (symbol "_") will trigger backtracking
    Cons <$> consId <*> many atom <* notFollowedBy (symbol "_")

-- Parse a command
command :: Parser Command
command =
  label
    "command"
    $ choice
      [ try letCommand,
        try letcCommand,
        try matchCommand,
        try patchCommand,
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
  label
    "Declaration"
    $ choice
      [ try fnDecl,
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
        -- @ operator sugar: x k @ fun === < Tuple x k |> fun >
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
    desugarAt fun args = Command  fun (Cons "Tuple" args)
    -- Desugar @ operator, when function is placed as co-expression
    coDesugarAt :: [Expr] -> Expr -> Command
    coDesugarAt [] _ = error "@ requires at least one argument"
    coDesugarAt args fun = Command (Cons "Tuple" args) fun

-- | Sugar for function definitions: fn/run
-- fn NAME ARGS* := COMMAND === NAME = mu[ Tuple ARGS... -> COMMAND ]
fnDecl :: Parser Decl
fnDecl =
  label "Sugared Declaration" $
    (\n args cmd -> Decl n (desugarFn args cmd))
      <$> (symbol "fn" *> varId)
      <*> many pattern
      <* symbol ":="
      <*> command
  where
    desugarFn :: [Pattern] -> Command -> Expr
    desugarFn args cmd =
      Mu [(ConsPattern "Tuple" args, cmd)]

-- | run COMMAND === main = mu[ halt -> COMMAND ]
runDecl :: Parser Decl
runDecl = do
  Decl "main" . desugarRun <$> (symbol "run" *> command)
  where
    desugarRun :: Command -> Expr
    desugarRun cmd = Mu [(VarPattern "halt", cmd)]

-- Sugar 4: have grammar
haveExpr :: Parser Expr
haveExpr = label "have expression" $ do
  _ <- symbol "have"
  bindings <- sepBy1 binding (symbol ",")
  _ <- symbol "in"
  desugarHave bindings <$> expr
  where
    desugarHave :: [(Either VarId CommandId, Either Expr Command)] -> Expr -> Expr
    desugarHave [] body = body
    desugarHave bindings body =
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

-- Cut-as-let: let VAR = EXPR in CMD => EXPR . { VAR -> CMD }
letCommand :: Parser Command
letCommand = label "let command" $ do
  _ <- symbol "let"
  pat <- pattern
  _ <- symbol "="
  e <- expr
  _ <- symbol "in"
  cmd <- command
  return $ Command e (Mu [(pat, cmd)])

-- letc: letc COVAR = COEXPR in CMD => { COVAR -> CMD } . COEXPR
letcCommand :: Parser Command
letcCommand = label "letc command" $ do
  _ <- symbol "letc"
  pat <- pattern
  _ <- symbol "="
  ce <- expr
  _ <- symbol "in"
  cmd <- command
  return $ Command (Mu [(pat, cmd)]) ce

-- match: match EXPR with | PAT1 -> CMD1 | PAT2 -> CMD2 ... => EXPR . { PAT1 -> CMD1 | PAT2 -> CMD2 ... }
matchCommand :: Parser Command
matchCommand = label "match command" $ do
  _ <- symbol "match"
  e <- expr
  _ <- symbol "with"
  _ <- optional (symbol "|")
  cases <- sepBy1 patternCase (symbol "|")
  return $ Command e (Mu cases)

-- patch: patch COEXPR with | PAT1 -> CMD1 | PAT2 -> CMD2 ... => { PAT1 -> CMD1 | PAT2 -> CMD2 ... } . COEXPR
patchCommand :: Parser Command
patchCommand = label "patch command" $ do
  _ <- symbol "patch"
  ce <- expr
  _ <- symbol "with"
  _ <- optional (symbol "|")
  cases <- sepBy1 patternCase (symbol "|")
  return $ Command (Mu cases) ce

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
findAndSubstCmdInExpr _ (Var v) =
  Var v

findAndSubstCmdInCmd :: CommandBinding -> Command -> Command
findAndSubstCmdInCmd (name, cmd) (Command expr1 expr2) =
  Command (findAndSubstCmdInExpr (name, cmd) expr1) (findAndSubstCmdInExpr (name, cmd) expr2)
findAndSubstCmdInCmd (name, cmd) (CommandVar cmdId) =
  if cmdId == name then cmd else CommandVar cmdId


-- Sugar: do...then grammar
doThenCommand :: Parser Command
doThenCommand = label "do/then command" $ do
  _ <- symbol "do"
  bindings <- sepBy (notFollowedBy (symbol "then") *> doBinding) (symbol ",")
  _ <- symbol "then"
  desugarDoThen bindings <$> command
  where
    doBinding :: Parser (Pattern, Expr)
    doBinding = do
      pat <- pattern
      _ <- symbol "<-"
      funCall <- expr
      return (pat, funCall)

    desugarDoThen :: [(Pattern, Expr)] -> Command -> Command
    desugarDoThen [] cmd = cmd
    desugarDoThen ((pat, funCall) : rest) cmd =
      -- FUN(ARGS...) . { PAT -> (rest) }
      Command funCall (Mu [(pat, desugarDoThen rest cmd)])
