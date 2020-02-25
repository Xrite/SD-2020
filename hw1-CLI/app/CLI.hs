module CLI
  ( runMainLoop
  )
  where

import           ShellImpl
import           Shell
import           Interpreter
import           System.IO


prefix = "> "

runMainLoop :: IO ()
runMainLoop = loadEnvironment >>= mainLoop

mainLoop env = do
  putStr prefix
  hFlush stdout
  line <- getLine
  (code, env') <- runShell (eval line) env
  if isTerminated env' then
    putStrLn $ "Program exited with " ++ show code
  else do
      putStr $ stdStream env'
      putStr $ errStream env'
      mainLoop $ dropStreams env'