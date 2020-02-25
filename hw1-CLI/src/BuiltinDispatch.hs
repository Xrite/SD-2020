module BuiltinDispatch
  ( dispatch
  )
where

import           Builtins.Cat
import           Builtins.Echo
import           Builtins.Exit
import           Builtins.Pwd
import           Builtins.Wc
import           Builtins.Grep
import           Builtins.Ls
import           Builtins.Cd
import           Control.Exception
import           Control.Monad.IO.Class
import           Shell
import           System.Exit


-- | Finds builtin command or execute or tries to run external process.
dispatch :: (Shell sh) => [String] -> sh ExitCode
dispatch [] = return $ ExitFailure 1
dispatch words | cmd == "cat"  = performCmd $ cat args
               | cmd == "echo" = performCmd $ echo args
               | cmd == "wc"   = performCmd $ wc args
               | cmd == "pwd"  = performCmd $ pwd args
               | cmd == "exit" = performCmd $ exit args
               | cmd == "grep" = performCmd $ grep args
               | cmd == "ls"   = performCmd $ ls args
               | cmd == "cd"   = performCmd $ cd args
               | otherwise     = performCmd $ runExternalProcess cmd args
 where
  cmd  = head words
  args = tail words

performCmd :: (ShellProcess sh, ShellExit sh) => sh ExitCode -> sh ExitCode
performCmd sh = sh >>= setExitCode
