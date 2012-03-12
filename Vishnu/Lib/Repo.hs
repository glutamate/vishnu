module Vishnu.Lib.Repo where

import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Directory
import Data.List
import Data.Ord

import Vishnu.Lib.Common

--does "dist/" exist and is it the most recently modified item?
modifiedSinceBuild :: String -> VisM Bool
modifiedSinceBuild s = do
  liftIO $ gotoHomeSubDir s
  conts <- liftIO $ getDirectoryContents "."
  entryTimes <- fmap (map fst . reverse . sortBy (comparing snd)) $ forM conts $ \entry -> do
    modTime <- liftIO $ getModificationTime entry
    return (entry, modTime)
  return $ decideModifiedSinceBuild entryTimes

decideModifiedSinceBuild ("." : rest) = decideModifiedSinceBuild rest
decideModifiedSinceBuild (".." : rest) = decideModifiedSinceBuild rest
decideModifiedSinceBuild (".git" : rest) = decideModifiedSinceBuild rest
decideModifiedSinceBuild ("dist" : rest) = False
decideModifiedSinceBuild _ = True

-- run git status
modifiedSinceCommit :: String -> VisM Bool
modifiedSinceCommit s = do
  liftIO $ gotoHomeSubDir s
  gitStat <- fmap lines $ sh "git status -suno"
  return $ not $ null $ gitStat

-- use github api
gitUpToDate :: String -> VisM Bool
gitUpToDate s = return False