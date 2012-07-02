module Vishnu.Cmds.Pull where

import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.Cmd
import Control.Monad.Reader
import Data.List
import System.Exit

import Vishnu.Lib.Common
import Vishnu.Lib.Repo


pull :: VisM ()
pull = perRepoFromArgs $ \repo -> liftIO $ do
    putStr $ repo ++ ": "
    gotoHomeSubDir repo
    system $ "git pull"

diff :: VisM ()
diff = perRepoFromArgs $ \repo -> liftIO $ do
    gotoHomeSubDir repo
    system $ "git diff"


showConf :: VisM ()
showConf = do
  VisS conf _ _ <- ask
  lift $ print conf

build :: VisM ()
build = perRepoFromArgs buildIt

buildIt repo =  do
    mod <- modifiedSinceBuild repo
    VisS _ _ args <- ask
    when (mod || "-f" `elem` args) $ liftIO $ do 
        gotoHomeSubDir repo
        let profstr = if "-p" `elem` args then " -p" else ""
        let runStr =  if "-g" `elem` args 
                         then "sudo cabal install --global" 
                         else "cabal install"
        ExitSuccess <-  system $ runStr ++ profstr
        return ()

status :: VisM ()
status = perRepoFromArgsPar $ \repo -> do
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
