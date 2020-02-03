{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShellImpl
    ( ShellImpl
    , Env
    , loadEnvironment
    , runShell
    , isTerminated
    , stdStream
    , errStream
    , dropStreams
    )
where

import           Control.Exception.Base
import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map                      as Map
import qualified System.Directory              as Dir
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           Shell
import           Control.Monad.State

data Env =
  Env
    { stdStream :: String
    , errStream :: String
    , variables :: Map.Map String String
    , isTerminated :: Bool
    }

emptyEnv = Env "" "" Map.empty False

lookupVarInEnv :: String -> Env -> IO String
lookupVarInEnv var env = case Map.lookup var (variables env) of
    Just s -> return s
    Nothing ->
        catch (getEnv var) ((\e -> return "") :: IOException -> IO String)

storeVarInEnv :: Env -> String -> String -> Env
storeVarInEnv env var val =
    env { variables = Map.insert var val (variables env) }

readStdinFromEnv :: Env -> (Env, String)
readStdinFromEnv env = (env { stdStream = "" }, stdStream env)

readStderrFromEnv :: Env -> (Env, String)
readStderrFromEnv env = (env { errStream = "" }, errStream env)

writeToEnvStdout :: Env -> String -> Env
writeToEnvStdout env str = env { stdStream = (stdStream env) ++ str }

writeToEnvStderr :: Env -> String -> Env
writeToEnvStderr env str = env { errStream = (errStream env) ++ str }

dropStreams :: Env -> Env
dropStreams env = env { stdStream = "", errStream = "" }



newtype ShellImpl a = ShellImpl {runSh :: (StateT Env IO) a} deriving (Functor, Applicative, Monad, MonadIO, MonadState Env)

exitCodeVariable = "?"

loadEnvironment :: IO Env
loadEnvironment = do
    vars <- getEnvironment
    return $ Env "" "" (Map.fromList vars) False

runShell :: ShellImpl ExitCode -> Env -> IO (ExitCode, Env)
runShell sh env = runStateT (runSh sh) env


instance ShellEnv ShellImpl where
    loadVar var = do
        env <- get
        case Map.lookup var (variables env) of
            Just s -> return s
            Nothing -> return ""

    storeVar var val = do
        env <- get
        put $ storeVarInEnv env var val 

    getCurrentDirectory = liftIO $ Dir.getCurrentDirectory

instance ShellIO ShellImpl where
    readFromStdin = do
        env <- get
        let (newEnv, input) = readStdinFromEnv env
        put newEnv
        return input

    readFromStderr = do
        env <- get
        let (newEnv, input) = readStderrFromEnv env
        put newEnv
        return input

    writeToStdout str = do
        env <- get
        put $ writeToEnvStdout env str

    writeToStderr str = do
        env <- get
        put $ writeToEnvStderr env str

instance ShellFileIO ShellImpl where
    getFileContents file = liftIO $ try (readFile file)

    writeToFile file str = liftIO $ try (writeFile file str)

instance ShellProcess ShellImpl where
    runExternalProcess cmd args = do
        env <- get
        let process = (proc cmd args) { env = Just . Map.toList $ variables env }
        input  <- readFromStdin
        either <-
            liftIO
                $ (try (readCreateProcessWithExitCode process input) :: IO
                        (Either IOException (ExitCode, String, String))
                  )
        case either of
            Left e -> do
                writeToStderr $ show e ++ "\n"
                return $ ExitFailure 127
            Right (code, stdout, stderr) -> do
                writeToStdout stdout
                return code

instance ShellExit ShellImpl where
    setExitCode ExitSuccess =
        storeVar exitCodeVariable "0" >> return ExitSuccess
    setExitCode (ExitFailure code) =
        storeVar exitCodeVariable (show code) >> (return $ ExitFailure code)

    exitShell code = do
        env <- get
        put $ env {isTerminated = True}
    
instance Shell ShellImpl