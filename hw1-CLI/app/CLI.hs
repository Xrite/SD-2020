module CLI
  ( runMainLoop
  ) where

import           Shell
import           Interpreter


prefix = "> "

runMainLoop :: IO ()
runMainLoop = mainLoop emptyEnv

mainLoop env = do
  putStr prefix
  line <- getLine
  res <- runShell (eval line) env
  case res of
    Left code -> putStrLn $ "Program exited with " ++ show code
    Right (env', _) -> do
      putStr $ stdStream env'
      putStr $ errStream env'
      mainLoop $ dropStreams env'
