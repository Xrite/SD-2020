-- | This module defines basic operations for shell.
module Shell where

import           System.Exit
import           Control.Exception

-- | The 'ShellEnv' class allows to work with environment. 
-- This class defines access to variables and directory.
class (Monad m) => ShellEnv m where
  loadVar :: String -> m String -- ^ Get variable by name
  storeVar :: String -> String -> m () -- ^ Store variable by name and given value
  getCurrentDirectory :: m String -- ^ Get current directory path
  getHomeDirectory :: m FilePath -- ^ Get home directory path
  setCurrentDirectory :: FilePath -> m (Either IOException ()) -- ^ Set current directory
  getDirectoryContents :: FilePath -> m [FilePath] -- ^ Get current directory's contents

-- | The 'ShellIO' class defines manipulations with standard inputs.
class (Monad m) => ShellIO m where
  readFromStdin :: m String -- ^ Get data from stdin
  readFromStderr :: m String -- ^ Get data from stderr
  writeToStdout :: String -> m () -- ^ Append string to stdout
  writeToStderr :: String -> m () -- ^ Append string to stderr

-- | Manipulations with files
class (Monad m) => ShellFileIO m where
  -- | Gets whole file contents. Returns either string or "IOException" if something went wrong
  getFileContents :: FilePath -> m (Either IOException String) 
  -- | Writes string to the file. Returns "IOException" if something went wrong.
  writeToFile :: FilePath -- ^ File
              -> String  -- ^ String to be written
              -> m (Either IOException ())

-- | External processes
class (Monad m) => ShellProcess m where
  -- | Runs external process
  runExternalProcess :: FilePath -- ^ Path to the program 
                    -> [String]  -- ^ Arguments to the program
                    -> m ExitCode -- ^ Exit code

-- | Exit codes
class (Monad m) => ShellExit m where
  -- | Sets exit code. Should return itself.
  setExitCode :: ExitCode -> m ExitCode
  -- | Signals that execution should be stopped. Sets an exit code.
  exitShell :: ExitCode -- ^ Shell exit code
            -> m ()

-- | Union of all classes in this module
class (ShellEnv m, ShellIO m, ShellFileIO m, ShellProcess m, ShellExit m) => Shell m
