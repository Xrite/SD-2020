module Builtins.Echo
    ( echo
    )
where

import           Data.List
import           Shell
import           System.Exit

-- | Prints arguments to the stdout.
echo :: (ShellIO sh) => [String] -> sh ExitCode
echo args =
    (writeToStdout $ intercalate " " args ++ "\n") >> return ExitSuccess
