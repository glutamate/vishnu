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
  VisS conf <- ask
  lift $ print conf

status :: VisM ()
status = perRepo $ \repo -> do
    liftIO $ putStr $ repo ++ ": "
    b <- modifiedSinceBuild repo
    when b $ liftIO $ putStr "Build "
    c <- modifiedSinceCommit repo
    when c $ liftIO $ putStr "Commit "
    when (not b && not c) $ liftIO $ putStr "OK"
    liftIO $ putStrLn ""
    return ()
