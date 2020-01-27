module Substitution where

import           Data.List
import           Parser
import           Shell
import           Text.Parsec
import           Text.Parsec.String

data IR
  = Str String
  | Var String
  deriving Show

substituteToken :: Token -> Shell String
substituteToken (Token parts) = concat <$> traverse substitutePart parts

substitutePart :: TokenPart -> Shell String
substitutePart (CharSequence s)  = substituteVars s
substitutePart (WeakQuoting s)   = substituteVars s
substitutePart (StrongQuoting s) = return s

substituteVars :: String -> Shell String
substituteVars str =
  case parse parseToIR "Parsing to IR" str of
    Left _    -> error "error in IR parsing"
    Right irs -> concat <$> traverse sub irs
  where
    sub (Str s) = return s
    sub (Var v) = loadVar v

parseToIR :: Parser [IR]
parseToIR = many1 $ (Str <$> many1 (noneOf "$")) <|> parseVar

parseVar :: Parser IR
parseVar = do
  char '$'
  Var <$> parseVariableName

parseCharSequence :: Parser String
parseCharSequence = do
  many (noneOf "$")

applySubstitution :: [Token] -> Shell [String]
applySubstitution ts = traverse substituteToken ts
