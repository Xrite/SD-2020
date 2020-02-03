module Builtins.Pwd
  ( pwd
  )
where

import           Control.Monad.IO.Class
import           Shell
import           System.Exit

-- | Prints current directory to the stdout.
pwd :: (ShellIO sh, ShellEnv sh) => [String] -> sh ExitCode
pwd args = do
  path <- getCurrentDirectory
  writeToStdout path
  return ExitSuccess
