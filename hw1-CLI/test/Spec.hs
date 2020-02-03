import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Parser
import           Data.List

genVariableName :: Gen String
genVariableName = listOf1 $ elements ['a'..'z']

genValidWord :: Gen String
genValidWord = listOf1 $ elements ['a'..'z']

genTokenPart :: Gen TokenPart
genTokenPart = oneof [CharSequence  <$> genValidWord, StrongQuoting <$> genValidWord, WeakQuoting  <$> genValidWord]

genToken :: Gen Token
genToken = Token <$> (shrink <$> listOf1 genTokenPart)
    where
        shrink [] = []
        shrink [x] = [x]
        shrink ((CharSequence s1):(CharSequence s2):rest) = shrink $ (CharSequence (s1 ++ s2)):rest
        shrink (x:rest) = x:(shrink rest)

genAssignment :: Gen Expression
genAssignment = do 
    var <- genVariableName
    val <- genToken
    return $ Assignment var val

genPipe :: Gen Expression
genPipe = Pipe <$> listOf (listOf1 genToken)

genExpression :: Gen Expression
genExpression = oneof [genAssignment, genPipe]

tokenToString :: Token -> String
tokenToString (Token t) = concat $ map convert t
    where
        convert (CharSequence s) = s
        convert (WeakQuoting s) = "\"" ++ s ++ "\""
        convert (StrongQuoting s) = "'" ++ s ++ "'"

expressionToString :: Expression -> String
expressionToString (Assignment var val) = var ++ "=" ++ tokenToString val
expressionToString (Pipe ts) = intercalate " | " (map tokenListToString ts)
    where
        tokenListToString l = intercalate " " (map tokenToString l)

prop_parseExpression = forAll genExpression $ \e -> parseExpression (expressionToString e) == Right e

main :: IO ()
main = hspec $ do
    describe "Test numbers" $ do
        it "comares 2 and 2" $ do
            2 `shouldBe` 2
    describe "parsing tests" $ do
        it "parse expression" $ 
            property prop_parseExpression  