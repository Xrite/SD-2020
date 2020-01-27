module Parser where

import           Text.Parsec
import           Text.Parsec.String

data TokenPart
  = CharSequence String
  | WeakQuoting String
  | StrongQuoting String
  deriving (Show)

data Token =
  Token [TokenPart]
  deriving (Show)

data Expression
  = Assignment String Token
  | Pipe [[Token]]
  deriving (Show)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  spaces
  return x

parseToken :: Parser Token
parseToken = lexeme $ do
  parts <- many1 (charSequence <|> weakQuoting <|> strongQuoting)
  return $ Token parts
  where
    charSequence = CharSequence <$> many1 (noneOf " \n\t'\"|")
    weakQuoting = do
      char '"'
      x <- many (noneOf "\"")
      char '"'
      return $ WeakQuoting x
    strongQuoting = do
      char '\''
      x <- many (noneOf "\'")
      char '\''
      return $ StrongQuoting x

parseVariableName :: Parser String
parseVariableName = do
  first <- letter
  rest <- many (letter <|> digit <|> char '?')
  return $ first : rest

parsePipe = do
  lexeme (char '|')
  return ()

parseAssignment :: Parser Expression
parseAssignment = do
  var <- parseVariableName
  char '='
  val <- parseToken
  return $ Assignment var val

parseExp = do
  spaces
  x <- try parseAssignment <|> (Pipe <$> sepBy (many1 parseToken) parsePipe)
  eof
  return x

parseExpression input = parse parseExp "(unknown)" input
