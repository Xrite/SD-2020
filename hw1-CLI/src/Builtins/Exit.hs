module Builtins.Exit where

import           Shell
import           System.Exit

exit :: [String] -> Shell ExitCode
exit args = do
  exitShell ExitSuccess
  return ExitSuccess
