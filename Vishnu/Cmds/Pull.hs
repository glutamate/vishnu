module Vishnu.Cmds.Pull where

import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.Cmd
import Control.Monad.Reader


import Vishnu.Lib.Common
import Vishnu.Lib.Repo

perRepo ma = do
  repos <- fmap words $ getConfig "localrepos" ""
  forM_ repos ma

pull :: VisM ()
pull = perRepo $ \repo -> liftIO $ do
    putStr $ repo ++ ": "
    gotoHomeSubDir repo
    system $ "git pull"

showConf :: VisM ()
showConf = do
  VisS conf _ <- ask
  lift $ print conf

build :: VisM ()
build = do
    VisS _ args <- ask
    case args of 
      [] -> perRepo buildIt
      repos -> mapM_ buildIt repos

buildIt repo = liftIO $ do
    gotoHomeSubDir repo
    system "sudo cabal install --global"
    return ()

status :: VisM ()
status = perRepo $ \repo -> do
    VisS _ args <- ask
    liftIO $ putStr $ repo ++ ": "
    b <- modifiedSinceBuild repo
    when b $ liftIO $ putStr "Build "
    c <- modifiedSinceCommit repo
    when c $ liftIO $ putStr "Commit "
    p <- pushPending repo
    when p $ liftIO $ putStr "Push "
    pl <- if "p" `elem` args 
             then pullPending repo
             else return False
    when p $ liftIO $ putStr "Push "
    when (not b && not c && not p && not pl) $ liftIO $ putStr "OK"
    liftIO $ putStrLn ""
    return ()
