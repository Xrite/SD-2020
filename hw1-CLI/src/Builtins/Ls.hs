module Builtins.Ls
  ( ls
  )
where

import           Control.Monad.IO.Class
import           Shell
import           System.Exit

-- | Prints current directory's contents to the stdout.
ls :: (ShellIO sh, ShellEnv sh) => [String] -> sh ExitCode
ls [] = do
  path <- getCurrentDirectory
  lsPath path

ls args = do
  results <- traverse lsPath args
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

lsPath path = do
  content <- getDirectoryContents path
  writeToStdout $ path ++ ":\n"
  traverse (\x -> do 
    writeToStdout $ x ++ "\n"
    ) content
  return ExitSuccess