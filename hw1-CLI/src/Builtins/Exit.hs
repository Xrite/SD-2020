module Builtins.Exit where

import           Shell
import           System.Exit

-- | Exits shell with success code.
exit :: (ShellExit sh) => [String] -> sh ExitCode
exit args = do
  exitShell ExitSuccess
  return ExitSuccess
