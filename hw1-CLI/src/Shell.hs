{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shell where

import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map               as Map
import qualified System.Directory       as Dir
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process

data Env =
  Env
    { stdStream :: String
    , errStream :: String
    , variables :: Map.Map String String
    }

emptyEnv = Env "" "" Map.empty

lookupVarInEnv :: Env -> String -> IO String
lookupVarInEnv env var =
  case Map.lookup var (variables env) of
    Just s -> return s
    Nothing ->
      catch (getEnv var) ((\e -> return "") :: IOException -> IO String)

storeVarInEnv :: Env -> String -> String -> Env
storeVarInEnv env var val = env {variables = Map.insert var val (variables env)}

readStdinFromEnv :: Env -> (Env, String)
readStdinFromEnv env = (env {stdStream = ""}, stdStream env)

readStderrFromEnv :: Env -> (Env, String)
readStderrFromEnv env = (env {errStream = ""}, errStream env)

writeToEnvStdout :: Env -> String -> Env
writeToEnvStdout env str = env {stdStream = (stdStream env) ++ str}

writeToEnvStderr :: Env -> String -> Env
writeToEnvStderr env str = env {errStream = (errStream env) ++ str}

dropStreams :: Env -> Env
dropStreams env = env {stdStream = "", errStream = ""}

data Shell a =
  Shell
    { runShell :: Env -> IO (Either ExitCode (Env, a))
    }
  deriving (Functor)

instance Applicative Shell where
  pure = return
  (<*>) = ap

instance Monad Shell where
  return x = Shell $ \env -> return $ Right (env, x)
  sh >>= f =
    Shell $ \env -> do
      res <- runShell sh env
      case res of
        Left code       -> return $ Left code
        Right (env', a) -> runShell (f a) env'

instance MonadIO Shell where
  liftIO io = Shell $ \env -> Right <$> ((,) <$> pure env <*> io)

loadVar :: String -> Shell String
loadVar var =
  Shell $ \env -> do
    val <- lookupVarInEnv env var
    return $ Right (env, val)

storeVar :: String -> String -> Shell ()
storeVar var val =
  Shell $ \env -> return $ Right (storeVarInEnv env var val, ())

exitCodeVariable = "?"

setExitCode :: ExitCode -> Shell ExitCode
setExitCode ExitSuccess = storeVar exitCodeVariable "0" >> return ExitSuccess
setExitCode (ExitFailure code) =
  storeVar exitCodeVariable (show code) >> (return $ ExitFailure code)

readFromStdin :: Shell String
readFromStdin =
  Shell $ \env ->
    let (newEnv, input) = readStdinFromEnv env
     in return $ Right (newEnv, input)

readFromStderr :: Shell String
readFromStderr =
  Shell $ \env ->
    let (newEnv, input) = readStderrFromEnv env
     in return $ Right (newEnv, input)

writeToStdout :: String -> Shell ()
writeToStdout str =
  Shell $ \env -> return $ Right (writeToEnvStdout env str, ())

writeToStderr :: String -> Shell ()
writeToStderr str =
  Shell $ \env -> return $ Right (writeToEnvStderr env str, ())

getFileContents :: FilePath -> Shell (Either IOException String)
getFileContents file = liftIO $ try (readFile file)

writeToFile :: FilePath -> String -> Shell (Either IOException ())
writeToFile file str = liftIO $ try (writeFile file str)

exitShell :: ExitCode -> Shell ()
exitShell code = Shell $ \env -> return $ Left code

getCurrentDirectory :: Shell String
getCurrentDirectory = liftIO $ Dir.getCurrentDirectory

runExternalProcess :: String -> [String] -> Shell ExitCode
runExternalProcess cmd args = do
  input <- readFromStdin
  either <-
    liftIO $
    (try (readProcessWithExitCode cmd args input) :: IO (Either IOException ( ExitCode
                                                                            , String
                                                                            , String)))
  case either of
    Left e -> do
      writeToStderr $ show e ++ "\n"
      return $ ExitFailure 127
    Right (code, stdout, stderr) -> do
      writeToStdout stdout
      return code
