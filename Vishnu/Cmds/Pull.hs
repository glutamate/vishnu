module Vishnu.Cmds.Pull where

import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.Cmd
import Control.Monad.Reader


import Vishnu.Lib.Common
import Vishnu.Lib.Repo

perRepoFromArgs ma = do
  args <- fmap moreArgs ask
--  liftIO $ print args
  case args of 
      [] -> perRepo ma
      repos -> mapM_ ma repos


perRepo ma = do
  repos <- fmap words $ getConfig "localrepos" ""
  forM_ repos ma

pull :: VisM ()
pull = perRepoFromArgs $ \repo -> liftIO $ do
    putStr $ repo ++ ": "
    gotoHomeSubDir repo
    system $ "git pull"

showConf :: VisM ()
showConf = do
  VisS conf _ _ <- ask
  lift $ print conf

build :: VisM ()
build = perRepoFromArgs buildIt

buildIt repo =  do
    mod <- modifiedSinceBuild repo
    when mod $ liftIO $ do gotoHomeSubDir repo
                           system "sudo cabal install --global"
                           return ()

status :: VisM ()
status = perRepoFromArgs $ \repo -> do
    VisS _ _ args <- ask
    liftIO $ putStr $ repo ++ ": "
    b <- modifiedSinceBuild repo
    when b $ liftIO $ putStr "Build "
    c <- modifiedSinceCommit repo
    when c $ liftIO $ putStr "Commit "
    p <- pushPending repo
    when p $ liftIO $ putStr "Push "
    pl <- if "-p" `elem` args 
             then pullPending repo
             else return False
    when pl $ liftIO $ putStr "Pull "
    when (not b && not c && not p && not pl) $ liftIO $ putStr "OK"
    liftIO $ putStrLn ""
    return ()
