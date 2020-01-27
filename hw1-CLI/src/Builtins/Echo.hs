module Builtins.Echo where

import           Data.List
import           Shell
import           System.Exit

echo :: [String] -> Shell ExitCode
echo args = (writeToStdout $ intercalate " " args) >> return ExitSuccess
