module Vishnu.Lib.RunVis where

import Control.Monad
import Vishnu.Lib.Common
import Data.List

import System.Environment

runVis :: [(String, VisM ())] -> IO ()
runVis cmds = do
  args <- getArgs
  case args of 
    [] -> help cmds
    arg0:rest -> dispatch arg0 rest cmds

help cmds = do
 putStrLn "vishnu: available commands:\n"
 forM_ cmds $ \(cmd,_) -> putStrLn ("  "++cmd) 
 putStrLn ""


dispatch :: String -> [String] -> [(String, VisM ())] -> IO ()
dispatch arg rest cmds 
   = case filter ((arg `isPrefixOf`) . fst) cmds of 
       (_,action):[] -> runVisM rest action
       [] -> putStrLn ("unknown command: "++arg) >> help cmds
       actions -> putStrLn ("ambiguous command: "++arg) >> help actions