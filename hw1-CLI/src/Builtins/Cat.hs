module Builtins.Cat
  ( cat
  ) where

import           Control.Exception
import           Shell
import           System.Exit

cat :: [String] -> Shell ExitCode
cat args = do
  results <- traverse catFile args
  return $
    if any (== Success) results
      then ExitSuccess
      else ExitFailure 1

data Status
  = Success
  | Failure
  deriving (Eq)

catFile :: String -> Shell Status
catFile file = do
  result <- getFileContents file
  case result of
    Left e -> do
      writeToStderr $ show e ++ "\n"
      return Failure
    Right contents -> do
      writeToStdout contents
      return Success
