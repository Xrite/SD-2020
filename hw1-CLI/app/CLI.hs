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

prompt = do
  putStr prefix
  hFlush stdout
  getLine

mainLoop env = do
  line         <- prompt
  (code, env') <- runShell (eval line) env
  if isTerminated env'
    then putStrLn $ "Program exited with " ++ show code
    else do
      putStr $ stdStream env'
      putStr $ errStream env'
      mainLoop $ dropStreams env'
