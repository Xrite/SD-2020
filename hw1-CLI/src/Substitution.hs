module Substitution
  ( applySubstitution
  )
where

import           Data.List
import           Parser
import           Shell
import           Text.Parsec
import           Text.Parsec.String

data IR
  = Str String
  | Var String
  deriving Show

substituteToken :: (ShellEnv m) => Token -> m [String]
substituteToken (Token parts)
  | shouldSplit = words . concat <$> traverse substitutePart parts
  | otherwise   = pure . concat <$> traverse substitutePart parts
 where
  shouldSplit = any (not . isQuoting) parts
  isQuoting (CharSequence _) = False
  isQuoting _                = True

substitutePart :: (ShellEnv m) => TokenPart -> m String
substitutePart (CharSequence  s) = substituteVars s
substitutePart (WeakQuoting   s) = substituteVars s
substitutePart (StrongQuoting s) = return s

substituteVars :: (ShellEnv m) => String -> m String
substituteVars str = case parse parseToIR "Parsing to IR" str of
  Left  _   -> error "error in IR parsing"
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

-- | Applies substitution using variables from 'ShellEnv' and splits tokens to words
applySubstitution :: (ShellEnv m) => [Token] -> m [String]
applySubstitution ts = concat <$> traverse substituteToken ts
