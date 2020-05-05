module Builtins.Cat
  ( cat
  ) where

import           Control.Exception
import           Shell
import           System.Exit

-- | Concatenates given files and prints them to the stdout.
-- If one of the files was missing or an error has occured during file reading,
-- it will print an error message to the stderr and will return 1.
cat :: (ShellFileIO sh, ShellIO sh) => [String] -> sh ExitCode
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

catFile :: (ShellFileIO sh, ShellIO sh) => String -> sh Status
catFile file = do
  result <- getFileContents file
  case result of
    Left e -> do
      writeToStderr $ show e ++ "\n"
      return Failure
    Right contents -> do
      writeToStdout contents
      return Success
