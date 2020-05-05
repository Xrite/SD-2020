module Builtins.Grep
    ( grep
    )
where

import           Shell
import           System.Exit
import           Data.Char                      ( toLower )
import           Text.Regex.Base
import           Text.Regex.TDFA
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Data.Maybe
import           Debug.Trace


-- | Print lines that matches pattern.
-- Searches for pattern in each file. Patterns are posix regex.
-- Possible flags:
-- -i, --ignore-case 
--      Ignore case when matching pattern.
-- -w, --word-regexp
--      Select only those lines containing matches that form whole words.
-- -A NUM, --after-context=NUM
--      Print NUM lines of trailing context after matching lines.
grep :: (ShellFileIO sh, ShellIO sh) => [String] -> sh ExitCode
grep args = do
    case execParserPure defaultPrefs opts args of
        Success           opts    -> onSuccess opts
        Failure           failure -> onFailure failure
        CompletionInvoked _       -> onCompletion
    where opts = info parser (fullDesc)

onSuccess :: (ShellIO sh, ShellFileIO sh) => Options -> sh ExitCode
onSuccess opts = case validateConfig cfg of
    Left  err -> writeToStderr err >> return (ExitFailure 1)
    Right _   -> afterValidate
  where
    cfg = Config { ignoreCase = optIgnoreCase opts
                 , wholeWords = optWholeWords opts
                 , pattern    = optPattern opts
                 , nextLines  = optNextLines opts
                 }
    getExitCode codes =
        if any (== (ExitFailure 2)) codes then ExitFailure 2 else last codes
    afterValidate = case optFiles opts of
        [] -> do
            readFromStdin >>= grepString cfg
        files -> do
            getExitCode <$> traverse (grepFile cfg) files

validateConfig :: Config -> Either String ()
validateConfig cfg = do
    checkNonNegativeNextLines
  where
    checkNonNegativeNextLines
        | nextLines cfg >= 0 = Right ()
        | otherwise          = Left "number of lines have to be non negative\n"


onFailure :: (ShellIO sh) => ParserFailure ParserHelp -> sh ExitCode
onFailure failure = do
    let (msg, code) = renderFailure failure "failure"
    writeToStderr (msg ++ "\n")
    return code

onCompletion :: (ShellIO sh) => sh ExitCode
onCompletion = do
    writeToStderr "No completion support\n"
    return $ ExitFailure 2

grepFile :: (ShellIO sh, ShellFileIO sh) => Config -> String -> sh ExitCode
grepFile cfg file = do
    res <- getFileContents file
    case res of
        Left err -> do
            writeToStderr (show err ++ "\n")
            return $ ExitFailure 2
        Right contents -> grepString cfg contents


grepString :: (ShellIO sh) => Config -> String -> sh ExitCode
grepString cfg string = do
    let allLines = lines string
    let results  = grepStrings cfg allLines
    if all (== NoMatch) results
        then return $ ExitFailure 1
        else do
            printResult cfg (-1) results allLines
            return ExitSuccess

grepStrings :: Config -> [String] -> [Result]
grepStrings cfg = map (buildMatch cfg)


parser :: Parser Options
parser =
    Options
        <$> switch (short 'i' <> long "ignore-case" <> help "Ignore case")
        <*> switch (short 'w' <> long "word-regexp" <> help "Match only words")
        <*> option
                auto
                (  short 'A'
                <> long "after-context"
                <> help "Number of strings to print after match"
                <> showDefault
                <> value 0
                <> metavar "n"
                )
        <*> strArgument (metavar "PATTERN")
        <*> many (strArgument (metavar "FILES..."))

printResult :: (ShellIO sh) => Config -> Int -> [Result] -> [String] -> sh ()
printResult cfg keep []       _        = return ()
printResult cfg keep (x : xs) (y : ys) = do
    if keep > 0
        then do
            writeToStdout $ y ++ "\n"
            case x of
                NoMatch -> printResult cfg (keep - 1) xs ys
                Result _ ->
                    printResult cfg (max (keep - 1) (nextLines cfg)) xs ys
        else case x of
            NoMatch  -> printResult cfg keep xs ys
            Result _ -> do
                if nextLines cfg > 0 && keep == 0
                    then writeToStdout "--\n"
                    else return ()
                writeToStdout $ y ++ "\n"
                printResult cfg (nextLines cfg) xs ys

buildMatch :: Config -> String -> Result
buildMatch cfg line =
    let mts = (preprocess line =~ pat :: Bool)
    in  if mts then Result [] else NoMatch
  where
    preprocess = if ignoreCase cfg then map toLower else id
    unified | ignoreCase cfg = map toLower (pattern cfg)
            | otherwise      = pattern cfg
    pat | wholeWords cfg = "\\b" ++ unified ++ "\\b"
        | otherwise      = unified


data Options = Options { optIgnoreCase :: Bool
                       , optWholeWords :: Bool
                       , optNextLines :: Int
                       , optPattern :: String
                       , optFiles :: [String]
                       }

data Result = Result { matches :: [(MatchOffset, MatchLength)] }
            | NoMatch
            deriving Eq

data Config = Config { ignoreCase :: Bool
                     , wholeWords :: Bool
                     , pattern :: String
                     , nextLines :: Int
                     }
