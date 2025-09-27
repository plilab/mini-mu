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

-- Parse idiom expressions *[expr] and [expr]
idiomExprWithHere :: Parser Expr
idiomExprWithHere = label "idiom expr with here" $ do
  DerefIdiomExpr <$> (symbol "*" *> squares command) -- deal with inner command

-- A idiom should be able to refer to the parent command
-- For example, in [add @ 3] @ 1 halt, the idiom can refer to "this @ 1 halt"
idiomExpr :: Parser Expr
idiomExpr = label "idiom expr" $ do
  IdiomExpr <$> squares command -- deal with inner command
      

-- atoms are naturally delimited expressions.
atom :: Parser Expr
atom =
  label "atom expr" $
    choice
      [ -- Sugar 7: Simplify mu[] as []
        try (Mu <$> curly (sepBy1 patternCase (symbol "|"))),
        try letExpr, -- TODO: move to atom so we allow: x @ X let y = ... in ...
        try idiomExprWithHere, -- *[expr] - adds implicit continuation "here"
        try idiomExpr, -- [expr] - basic idiom form
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
        try incompleteCons,
        atom
      ]

completeCons :: Parser Expr
completeCons =
  label "complete constructor" $
    -- notFollowedBy (symbol "_") will trigger backtracking
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
    desugarAt fun args = Command  fun (Cons "Ap" args)
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
findAndSubstExprInExpr (name, e) (IdiomExpr cmd) =
  IdiomExpr (findAndSubstExprInCmd (name, e) cmd)
findAndSubstExprInExpr (name, e) (DerefIdiomExpr cmd) =
  DerefIdiomExpr (findAndSubstExprInCmd (name, e) cmd)
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
findAndSubstCmdInExpr (name, cmd) (IdiomExpr c) =
  IdiomExpr (findAndSubstCmdInCmd (name, cmd) c)
findAndSubstCmdInExpr (name, cmd) (DerefIdiomExpr c) =
  DerefIdiomExpr (findAndSubstCmdInCmd (name, cmd) c)
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


-- expandCommandTree :: Command -> Command
-- expandCommandTree prt@(Command _ _) = expandCommandTreeAux prt 0
--   where
--     expandCommandTreeAux :: Command -> Int -> Command
--     expandCommandTreeAux (Command expr1 expr2) level =
--       case findFirstIdiom expr1 of
--         Just (DerefIdiomExpr cmd) ->
--           let thisVarName = "this" ++ show level
--               expandedInnerCmd = expandCommandTreeAux cmd (level + 1)
--               hereSubstituted = substituteHereInCommand expandedInnerCmd (Var thisVarName)
--               thisDefinition = Mu [(VarPattern "res", Command expr2 (Var "res"))]
--               resultExpr = case hereSubstituted of
--                 Command cmdExpr1 _ -> cmdExpr1
--                 _ -> error "Expected Command after here substitution"
--               newExpr1 = replaceFirstIdiom expr1 resultExpr
--           in expandCommandTreeAux (Command newExpr1 thisDefinition) level
--         Just (IdiomExpr cmd) ->
--           let expandedInnerCmd = expandCommandTreeAux cmd (level + 1)
--               newExpr1 = replaceFirstIdiom expr1 (IdiomExpr expandedInnerCmd)
--           in expandCommandTreeAux (Command newExpr1 expr2) level
--         Just _ -> error "Should not reach here"
--         Nothing ->
--           case findFirstIdiom expr2 of
--             Just (DerefIdiomExpr cmd) ->
--               let thisVarName = "this" ++ show level
--                   expandedInnerCmd = expandCommandTreeAux cmd (level + 1)
--                   hereSubstituted = substituteHereInCommand expandedInnerCmd (Var thisVarName)
--                   thisDefinition = Mu [(VarPattern "res", Command expr1 (Var "res"))]
--                   resultExpr = case hereSubstituted of
--                     Command cmdExpr1 _ -> cmdExpr1
--                     _ -> error "Expected Command after here substitution"
--                   newExpr2 = replaceFirstIdiom expr2 resultExpr
--               in expandCommandTreeAux (Command thisDefinition newExpr2) level
--             Just (IdiomExpr cmd) ->
--               let expandedInnerCmd = expandCommandTreeAux cmd (level + 1)
--                   newExpr2 = replaceFirstIdiom expr2 (IdiomExpr expandedInnerCmd)
--               in expandCommandTreeAux (Command expr1 newExpr2) level
--             Just _ -> error "Should not reach here"
--             Nothing -> Command expr1 expr2
--     expandCommandTreeAux (CommandVar cmdId) _ = CommandVar cmdId
-- expandCommandTree var = var

-- findFirstIdiom :: Expr -> Maybe Expr
-- findFirstIdiom idm@(DerefIdiomExpr _) = Just idm
-- findFirstIdiom idm@(IdiomExpr _) = Just idm
-- findFirstIdiom (Cons _ exprs) = findFirstInArgs exprs
-- findFirstIdiom (IncompleteCons _ args) = findFirstInIncompleteArgs args
-- findFirstIdiom (Mu branches) = findFirstInBranches branches
-- findFirstIdiom _ = Nothing

-- findFirstInArgs :: [Expr] -> Maybe Expr
-- findFirstInArgs [] = Nothing
-- findFirstInArgs (e:es) = case findFirstIdiom e of
--   Just found -> Just found
--   Nothing -> findFirstInArgs es

-- findFirstInIncompleteArgs :: [Either Expr HoleExpr] -> Maybe Expr
-- findFirstInIncompleteArgs [] = Nothing
-- findFirstInIncompleteArgs (Left e:es) = case findFirstIdiom e of
--   Just found -> Just found
--   Nothing -> findFirstInIncompleteArgs es
-- findFirstInIncompleteArgs (Right _:es) = findFirstInIncompleteArgs es

-- findFirstInBranches :: [(Pattern, Command)] -> Maybe Expr
-- findFirstInBranches [] = Nothing
-- findFirstInBranches ((_, Command e1 e2):rest) =
--   case findFirstIdiom e1 of
--     Just found -> Just found
--     Nothing -> case findFirstIdiom e2 of
--       Just found -> Just found
--       Nothing -> findFirstInBranches rest
-- findFirstInBranches ((_, CommandVar _):rest) = findFirstInBranches rest

-- replaceFirstIdiom :: Expr -> Expr -> Expr
-- replaceFirstIdiom (DerefIdiomExpr _) replacement = replacement
-- replaceFirstIdiom (IdiomExpr _) replacement = replacement
-- replaceFirstIdiom (Cons cid exprs) replacement =
--   Cons cid (replaceFirstInList exprs replacement)
-- replaceFirstIdiom (IncompleteCons cid args) replacement =
--   IncompleteCons cid (replaceFirstInEitherList args replacement)
-- replaceFirstIdiom (Mu branches) replacement =
--   Mu (replaceFirstInBranches branches replacement)
-- replaceFirstIdiom e _ = e

-- replaceFirstInList :: [Expr] -> Expr -> [Expr]
-- replaceFirstInList [] _ = []
-- replaceFirstInList (e:es) replacement =
--   case findFirstIdiom e of
--     Just _ -> replaceFirstIdiom e replacement : es
--     Nothing -> e : replaceFirstInList es replacement

-- replaceFirstInEitherList :: [Either Expr HoleExpr] -> Expr -> [Either Expr HoleExpr]
-- replaceFirstInEitherList [] _ = []
-- replaceFirstInEitherList (Left e:es) replacement =
--   case findFirstIdiom e of
--     Just _ -> Left (replaceFirstIdiom e replacement) : es
--     Nothing -> Left e : replaceFirstInEitherList es replacement
-- replaceFirstInEitherList (Right h:es) replacement = Right h : replaceFirstInEitherList es replacement

-- replaceFirstInBranches :: [(Pattern, Command)] -> Expr -> [(Pattern, Command)]
-- replaceFirstInBranches [] _ = []
-- replaceFirstInBranches ((pat, Command e1 e2):rest) replacement =
--   case findFirstIdiom e1 of
--     Just _ -> (pat, Command (replaceFirstIdiom e1 replacement) e2) : rest
--     Nothing -> case findFirstIdiom e2 of
--       Just _ -> (pat, Command e1 (replaceFirstIdiom e2 replacement)) : rest
--       Nothing -> (pat, Command e1 e2) : replaceFirstInBranches rest replacement
-- replaceFirstInBranches ((pat, cmd@(CommandVar _)):rest) replacement =
--   (pat, cmd) : replaceFirstInBranches rest replacement

-- substituteHereInCommand :: Command -> Expr -> Command
-- substituteHereInCommand (Command expr1 expr2) hereExpr =
--   Command (substituteHereInExpr expr1 hereExpr) (substituteHereInExpr expr2 hereExpr)
-- substituteHereInCommand cmd _ = cmd

-- substituteHereInExpr :: Expr -> Expr -> Expr
-- substituteHereInExpr (Var "here") hereExpr = hereExpr
-- substituteHereInExpr (Var v) _ = Var v
-- substituteHereInExpr (Cons cid exprs) hereExpr =
--   Cons cid (map (\e -> substituteHereInExpr e hereExpr) exprs)
-- substituteHereInExpr (IncompleteCons cid args) hereExpr =
--   IncompleteCons cid (map (either (Left . substituteHereInExpr hereExpr) Right) args)
-- substituteHereInExpr (IdiomExpr cmd) hereExpr =
--   IdiomExpr (substituteHereInCommand cmd hereExpr)
-- substituteHereInExpr (DerefIdiomExpr cmd) hereExpr =
--   DerefIdiomExpr (substituteHereInCommand cmd hereExpr)
-- substituteHereInExpr (Mu branches) hereExpr =
--   Mu (map (\(pat, cmd) -> (pat, substituteHereInCommand cmd hereExpr)) branches)
