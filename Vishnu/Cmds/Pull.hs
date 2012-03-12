module Vishnu.Cmds.Pull where

import Control.Monad
import System.Directory
import System.Cmd

repos = words "tnutils samfun gnewplot baysig"

pull = forM_ repos $ \repo -> do
  putStr $ repo ++ ": "
  setCurrentDirectory $ "/home/tomn/"++repo  
  system $ "git pull"