module Vishnu.Lib.Repo where

import Control.Monad
import System.Environment


--does "dist/" exist and is it the most recently modified item?
modifiedSinceBuild :: String -> IO Bool

-- run git status
modifiedSinceCommit :: String -> IO Bool

-- use github api
gitUpToDate :: String -> IO Bool