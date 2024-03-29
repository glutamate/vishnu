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
  newer <- fmap (map (take 4 . drop 2) . lines ) $ sh "find -cnewer dist"
  return $ not $ null $ filter noDistGit newer

{-  conts <- liftIO $ getDirectoryContents "."
  entryTimes <- fmap (map fst . reverse . sortBy (comparing snd)) $ forM conts $ \entry -> do
    modTime <- liftIO $ getModificationTime entry
    return (entry, modTime)
  liftIO $ print entryTimes
  return $ decideModifiedSinceBuild entryTimes

decideModifiedSinceBuild ("." : rest) = decideModifiedSinceBuild rest
decideModifiedSinceBuild (".." : rest) = decideModifiedSinceBuild rest
decideModifiedSinceBuild (".git" : rest) = decideModifiedSinceBuild rest
decideModifiedSinceBuild ("dist" : rest) = False
decideModifiedSinceBuild _ = True -}

noDistGit "dist" = False
noDistGit ".git" = False
noDistGit _ = True

-- run git status
modifiedSinceCommit :: String -> VisM Bool
modifiedSinceCommit s = do
  liftIO $ gotoHomeSubDir s
  gitStat <- fmap lines $ sh "git status -suno"
  return $ not $ null $ gitStat

pushPending :: String -> VisM Bool
pushPending s = do
  liftIO $ gotoHomeSubDir s
  gitStat <- fmap lines $ sh "git status"
  return $ any ("Your branch is ahead" `isInfixOf`) gitStat

pullPending :: String -> VisM Bool
pullPending s = do
  liftIO $ gotoHomeSubDir s
  gitStat <- fmap lines $ shProc "git" (words "remote show origin")
  return $ any ("local out of date" `isInfixOf`) gitStat


-- use github api
gitUpToDate :: String -> VisM Bool
gitUpToDate s = return False
