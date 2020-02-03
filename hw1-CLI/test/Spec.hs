{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Parser
import           Substitution
import           Shell
import           Control.Monad.Identity

newtype IdEnv a = IdEnv (Identity a) deriving (Functor, Applicative, Monad, Eq, Show)

instance ShellEnv IdEnv where
    loadVar var = pure var
    storeVar var val = return ()
    getCurrentDirectory = undefined 

oneWord :: String -> IdEnv [String]
oneWord s = pure [s]

main :: IO ()
main = hspec $ do
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
            applySubstitution (wrap "$x $y") `shouldBe` oneWord "x y"
            applySubstitution (wrap "$x $x") `shouldBe` oneWord "x x"
            applySubstitution (wrap "$x$y") `shouldBe` oneWord "xy"
            applySubstitution (wrap "$x   $y") `shouldBe` oneWord "x   y"
            applySubstitution (wrap "$xyz") `shouldBe` oneWord "xyz"
            applySubstitution (wrap "$x z") `shouldBe` oneWord "x z"
    describe "parsing tests" $ do
        it "parse expression" $ 
            property prop_parseExpression