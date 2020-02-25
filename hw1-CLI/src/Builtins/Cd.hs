module Builtins.Cd
  ( cd
  )
where

import           Control.Monad.IO.Class
import           Shell
import           System.Exit
import           System.FilePath

-- | Sets current directory to the given.
cd :: (ShellIO sh, ShellEnv sh) => [String] -> sh ExitCode
cd [] = do
  writeToStderr "No argument"
  return $ ExitFailure 1

cd (path:rest) = do
  homeDir <- getHomeDirectory
  let (first:rest) = splitDirectories path
  let first' = if first == "~" then homeDir else first
  let path = joinPath (first':rest)
  res <- setCurrentDirectory path
  case res of
    Left err -> do
      writeToStderr $ (show err ++ "\n")
      return $ ExitFailure 1
    Right () -> do
      return ExitSuccess
