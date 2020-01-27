module Builtins.Wc
  ( wc
  ) where

import           Data.List
import           Shell
import           System.Exit

wc :: [String] -> Shell ExitCode
wc [] = do
  input <- readFromStdin
  let (lines, words, bytes) = countAll input
  writeToStdout $
    (show lines) ++ " " ++ (show words) ++ " " ++ (show bytes) ++ "\n"
  return ExitSuccess
wc args = do
  results <- traverse wcFile args
  return $
    if any (== Success) results
      then ExitSuccess
      else ExitFailure 1

data Status
  = Success
  | Failure
  deriving (Eq)

wcFile :: String -> Shell Status
wcFile file = do
  result <- getFileContents file
  case result of
    Left e -> do
      writeToStderr $ show e ++ "\n"
      return Failure
    Right contents -> do
      let (lines, words, bytes) = countAll contents
      writeToStdout $
        (show lines) ++ " " ++ (show words) ++ " " ++ (show bytes) ++ "\n"
      return Success

countAll str = (linesCount, wordsCount, bytesCount)
  where
    linesCount = length $ lines str
    wordsCount = length $ words str
    bytesCount = length str
