module Interpreter where

import           BuiltinDispatch
import           Parser
import           Shell
import           Substitution
import           System.Exit
import           Text.Parsec
import           Text.Parsec.String

eval :: String -> Shell ExitCode
eval cmd = do
  case parseExpression cmd of 
    Left err -> do
      writeToStdout $ show err
      return $ ExitFailure 2
    Right expr -> evalExpression expr

evalExpression :: Expression -> Shell ExitCode
evalExpression (Assignment var token) = do
  strs <- applySubstitution [token]
  storeVar var (head strs)
  return ExitSuccess
evalExpression (Pipe tokens) = evalPipe tokens

evalPipe :: [[Token]] -> Shell ExitCode
evalPipe [] = return ExitSuccess
evalPipe ts = do
  codes <- traverse evalTokensList ts
  return $ last codes

evalTokensList :: [Token] -> Shell ExitCode
evalTokensList tokens = do
  exitCode <- applySubstitution tokens >>= dispatch
  setExitCode exitCode
