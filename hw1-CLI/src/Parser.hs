module Parser
  ( parseVariableName
  , parseExpression
  , prop_parseExpression
  , Token (..)
  , TokenPart (..)
  , Expression (..)
  )
where

import           Text.Parsec
import           Text.Parsec.String
import           Test.QuickCheck
import           Data.List                      ( intercalate )

-- | Part of a token. Each part is one of unquoted char sequence, 
-- double quoted char sequence or single quoted char sequence.
data TokenPart
  = CharSequence String -- ^ Unqouted char sequence
  | WeakQuoting String -- ^ Double quoted char sequence
  | StrongQuoting String -- ^ Single quoted char sequence
  deriving (Show, Eq)

-- | Representation of a token. Each token consists of one or more 'TokenPart'.
data Token =
  Token [TokenPart] 
  deriving (Show, Eq)

-- | Representation of expression. Can be one of assignment or pipe.
data Expression
  = Assignment String Token -- ^ Assignment to variable
  | Pipe [[Token]] -- ^ Sequence of commands. Each command consists of list of tokens.
  deriving (Show, Eq)

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

-- | Parse variable name.
parseVariableName :: Parser String
parseVariableName = do
  name <- many1 (letter <|> digit <|> oneOf "_?")
  return $ name

parsePipe = do
  lexeme (char '|')
  return ()

parseAssignment :: Parser Expression
parseAssignment = do
  var <- parseVariableName
  char '='
  val <- parseToken
  return $ Assignment var val

parseExp :: Parser Expression
parseExp = do
  spaces
  x <- try parseAssignment <|> (Pipe <$> sepBy (many1 parseToken) parsePipe)
  eof
  return x

-- | Parse expression from string.
parseExpression :: String -> Either ParseError Expression
parseExpression input = parse parseExp "(unknown)" input

genVariableName :: Gen String
genVariableName = listOf1 $ elements ['a'..'z']

genValidWord :: Gen String
genValidWord = listOf1 $ elements ['a'..'z']

genTokenPart :: Gen TokenPart
genTokenPart = oneof [CharSequence  <$> genValidWord, StrongQuoting <$> genValidWord, WeakQuoting  <$> genValidWord]

genToken :: Gen Token
genToken = Token <$> (shrink <$> listOf1 genTokenPart)
    where
        shrink [] = []
        shrink [x] = [x]
        shrink ((CharSequence s1):(CharSequence s2):rest) = shrink $ (CharSequence (s1 ++ s2)):rest
        shrink (x:rest) = x:(shrink rest)

genAssignment :: Gen Expression
genAssignment = do 
    var <- genVariableName
    val <- genToken
    return $ Assignment var val

genPipe :: Gen Expression
genPipe = Pipe <$> listOf (listOf1 genToken)

genExpression :: Gen Expression
genExpression = oneof [genAssignment, genPipe]

tokenToString :: Token -> String
tokenToString (Token t) = concat $ map convert t
    where
        convert (CharSequence s) = s
        convert (WeakQuoting s) = "\"" ++ s ++ "\""
        convert (StrongQuoting s) = "'" ++ s ++ "'"

expressionToString :: Expression -> String
expressionToString (Assignment var val) = var ++ "=" ++ tokenToString val
expressionToString (Pipe ts) = intercalate " | " (map tokenListToString ts)
    where
        tokenListToString l = intercalate " " (map tokenToString l)

-- | Printed expressions parse to the same expressions
prop_parseExpression = forAll genExpression $ \e -> parseExpression (expressionToString e) == Right e