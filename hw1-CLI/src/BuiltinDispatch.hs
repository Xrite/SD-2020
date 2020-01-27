module BuiltinDispatch where

import           Builtins.Cat
import           Builtins.Echo
import           Builtins.Exit
import           Builtins.Pwd
import           Builtins.Wc
import           Control.Exception
import           Control.Monad.IO.Class
import           Shell
import           System.Exit


dispatch :: [String] -> Shell ExitCode
dispatch [] = return $ ExitFailure 1
dispatch words
  | cmd == "cat" = performCmd $ cat args
  | cmd == "echo" = performCmd $ echo args
  | cmd == "wc" = performCmd $ wc args
  | cmd == "pwd" = performCmd $ pwd args
  | cmd == "exit" = performCmd $ exit args
  | otherwise = performCmd $ runExternalProcess cmd args
  where
    cmd = head words
    args = tail words

performCmd :: Shell ExitCode -> Shell ExitCode
performCmd sh = sh >>= setExitCode