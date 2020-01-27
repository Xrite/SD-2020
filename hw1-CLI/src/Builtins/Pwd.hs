module Builtins.Pwd where

import           Control.Monad.IO.Class
import           Shell
import           System.Exit

pwd :: [String] -> Shell ExitCode
pwd args = do
  path <- getCurrentDirectory
  writeToStdout path
  return ExitSuccess
