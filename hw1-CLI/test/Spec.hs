{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Parser
import           Substitution
import           Shell
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Map.Lazy                 as Map
import           Builtins.Cat
import           Builtins.Echo
import           Builtins.Exit
import           Builtins.Pwd
import           Builtins.Wc
import           System.Exit
import           System.Process                as Proc
import Control.Exception

newtype IdEnv a = IdEnv (Identity a) deriving (Functor, Applicative, Monad, Eq, Show)

instance ShellEnv IdEnv where
    loadVar var = pure var
    storeVar var val = return ()
    getCurrentDirectory = undefined 

oneWord :: String -> IdEnv [String]
oneWord s = pure [s]

main :: IO ()
main = hspec $ do
    substitutionSpec
    catSpec
    echoSpec
    exitSpec
    pwdSpec
    wcSpec
    externalProcessSpec
    parsingSpec

substitutionSpec = do
    describe "Test substitution" $ do
        it "does not substitute strong quoting" $ do
            let wrap s = [Token [StrongQuoting s]]
            applySubstitution (wrap "$x") `shouldBe` oneWord "$x"
            applySubstitution (wrap "$x$x") `shouldBe` oneWord "$x$x"
            applySubstitution (wrap "$x   $y") `shouldBe` oneWord "$x   $y"
            applySubstitution (wrap "$x y") `shouldBe` oneWord "$x y"
        it "does substitute weak quoting" $ do
            let wrap s = [Token [WeakQuoting s]]
            applySubstitution (wrap "$x") `shouldBe` oneWord "x"
            applySubstitution (wrap "$x $y") `shouldBe` oneWord "x y"
            applySubstitution (wrap "$x $x") `shouldBe` oneWord "x x"
            applySubstitution (wrap "$x$y") `shouldBe` oneWord "xy"
            applySubstitution (wrap "$x   $y") `shouldBe` oneWord "x   y"
            applySubstitution (wrap "$xyz") `shouldBe` oneWord "xyz"
            applySubstitution (wrap "$x z") `shouldBe` oneWord "x z"
        it "does substitute unqouted strings" $ do
            let wrap s = [Token [CharSequence s]]
            applySubstitution (wrap "$x") `shouldBe` oneWord "x"
            applySubstitution (wrap "$xyz") `shouldBe` oneWord "xyz"
            applySubstitution (wrap "$x$x") `shouldBe` oneWord "xx"
            applySubstitution (wrap "$x$y") `shouldBe` oneWord "xy"

parsingSpec = do
    describe "parsing tests" $ do
        it "parses printed expression to itself" $ 
            property prop_parseExpression


data FakeShellEnv = FakeShellEnv {stdin :: String, stderr :: String, files :: Map String String}

emptyFakeShell = FakeShellEnv "" "" empty 

newtype MockShell a = MockShellFileIO {runMockShell :: ((StateT FakeShellEnv IO)  a)}
    deriving (Functor, Applicative, Monad, MonadState FakeShellEnv, MonadIO)

instance ShellFileIO MockShell where
    getFileContents file = do
        env <- get
        let (Just res) = Map.lookup file (files env)
        return $ Right res
    
    writeToFile file cont = do
        env <- get 
        put $ env {files = insert file cont (files env)}
        return $ Right ()

        
instance ShellIO MockShell where
    readFromStdin = do
        env <- get
        put $ env {stdin = ""}
        return $ stdin env

    readFromStderr = do
        env <- get
        put $ env {stderr = ""}
        return $ stderr env

    writeToStdout str = do
        env <- get
        put $ env {stdin = stdin env ++ str}

    writeToStderr str = do
        env <- get
        put $ env {stderr = stderr env ++ str}

instance ShellProcess MockShell where
    runExternalProcess cmd args = do
        env <- get
        let process = (proc cmd args)
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

instance ShellExit MockShell where
    setExitCode code = return code

    exitShell code = return ()

instance ShellEnv MockShell where
    loadVar var = undefined 

    storeVar var val = undefined 
    
    getCurrentDirectory = return "/"


catSpec = do
    describe "Test cat" $ do
        let fs = fromList [("file1", "aba"), ("file2", "caba")]
        let env = emptyFakeShell {files = fs}
        it "reads one file" $ do
            let action = runMockShell (cat ["file1"]) 
            stdin <$> (execStateT action env) `shouldReturn` "aba"
        it "reads several files" $ do
            let action = runMockShell (cat ["file1", "file2"]) 
            stdin <$> (execStateT action env) `shouldReturn` "abacaba"

echoSpec = do
    describe "Test echo" $ do
        let fs = fromList [("file1", "aba"), ("file2", "caba")]
        let env = emptyFakeShell {files = fs}

        it "prints newline when no arguments given and returns success" $ do
            let action = runMockShell (echo []) 
            let resIO = execStateT action env
            let codeIO = evalStateT action env
            stdin <$> resIO `shouldReturn` "\n"
            stderr <$> resIO `shouldReturn` ""
            codeIO `shouldReturn` ExitSuccess

        it "prints one argument and returns success" $ do
            let action = runMockShell (echo ["aa"]) 
            let resIO = execStateT action env
            let codeIO = evalStateT action env
            stdin <$> resIO `shouldReturn` "aa\n"
            stderr <$> resIO `shouldReturn` ""
            codeIO `shouldReturn` ExitSuccess

        it "prints several arguments and returns success" $ do
            let action = runMockShell (echo ["aa", "bb", "cc"]) 
            let resIO = execStateT action env
            let codeIO = evalStateT action env
            stdin <$> resIO `shouldReturn` "aa bb cc\n"
            stderr <$> resIO `shouldReturn` ""
            codeIO `shouldReturn` ExitSuccess

exitSpec = do
    describe "Test exit" $ do
        let fs = fromList [("file1", "aba"), ("file2", "caba")]
        let env = emptyFakeShell {files = fs}

        it "returns success code when no argument was given and returns success" $ do
            let action = runMockShell (exit []) 
            let resIO = execStateT action env
            let codeIO = evalStateT action env
            stdin <$> resIO `shouldReturn` ""
            stderr <$> resIO `shouldReturn` ""
            codeIO `shouldReturn` ExitSuccess

pwdSpec = do
    describe "Test pwd" $ do
        let env = emptyFakeShell

        it "prints directory and returns success" $ do
            let action = runMockShell (pwd []) 
            let resIO = execStateT action env
            let codeIO = evalStateT action env
            stdin <$> resIO `shouldReturn` "/"
            stderr <$> resIO `shouldReturn` ""
            codeIO `shouldReturn` ExitSuccess

wcSpec = do
    describe "Test wc" $ do
        let fs = fromList [("file1", "aba caba")
                        , ("file2", "caba\n228")
                        , ("file3", "ora ora ora\nmuda muda muda\nora ora ora\nmuda muda muda")]

        it "counts in one file and returns success" $ do
            let env = emptyFakeShell {files = fs}
            let action = runMockShell (wc ["file2"])
            let resIO = execStateT action env
            let codeIO = evalStateT action env
            stdin <$> resIO `shouldReturn` "2 2 8\n"
            stderr <$> resIO `shouldReturn` ""
            codeIO `shouldReturn` ExitSuccess

        it "counts in several files and returns success" $ do
            let env = emptyFakeShell {files = fs}
            let action = runMockShell (wc ["file1", "file2", "file3"])
            let resIO = execStateT action env
            let codeIO = evalStateT action env
            stdin <$> resIO `shouldReturn` "1 2 8\n2 2 8\n4 12 53\n"
            stderr <$> resIO `shouldReturn` ""
            codeIO `shouldReturn` ExitSuccess
    
externalProcessSpec = do
    describe "Test running external processes" $ do
        it "runs git --help and returns success" $ do
            let env = emptyFakeShell
            let action = runMockShell (runExternalProcess "git" ["--help"])
            let codeIO = evalStateT action env
            codeIO `shouldReturn` ExitSuccess 
            
