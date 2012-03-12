module Vishnu.Lib.RunVis where

import Control.Monad
import System.Environment

runVis :: [(String, IO ())] -> IO ()
runVis cmds = do
  args <- getArgs
  case args of 
    [] -> help cmds
    arg0:rest -> dispatch arg0 rest cmds

help cmds = do
 putStrLn "available commands:\n"
 forM_ cmds $ \(cmd,_) -> putStrLn ("  "++cmd) 

dispatch arg rest cmds 
   = case lookup arg cmds of 
       Just action -> action
       Nothing -> putStrLn ("unknown command: "++arg) >> help cmds