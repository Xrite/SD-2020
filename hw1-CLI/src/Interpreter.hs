module Interpreter
  ( eval
  )
where

import           BuiltinDispatch
import           Parser
import           Shell
import           Substitution
import           System.Exit
import           Text.Parsec
import           Text.Parsec.String

-- | Build and run command from textual representation (e.g. interpretation).
eval :: (Shell sh) => String -- ^ command
                   -> sh ExitCode -- ^ return code
eval cmd = do
  case parseExpression cmd of
    Left err -> do
      writeToStdout $ show err
      return $ ExitFailure 2
    Right expr -> evalExpression expr

evalExpression :: (Shell sh) => Expression -> sh ExitCode
evalExpression (Assignment var token) = do
  strs <- applySubstitution [token]
  storeVar var (head strs)
  return ExitSuccess
evalExpression (Pipe tokens) = evalPipe tokens

evalPipe :: (Shell sh) => [[Token]] -> sh ExitCode
evalPipe [] = return ExitSuccess
evalPipe ts = do
  codes <- traverse evalTokensList ts
  return $ last codes

evalTokensList :: (Shell sh) => [Token] -> sh ExitCode
evalTokensList tokens = do
  exitCode <- applySubstitution tokens >>= dispatch
  setExitCode exitCode


