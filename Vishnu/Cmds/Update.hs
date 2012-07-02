module Vishnu.Cmds.Update where

import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.Cmd
import Control.Monad.Reader
import Data.List
import System.Exit
import qualified Data.HashMap.Lazy as H


import Vishnu.Lib.Common
import Vishnu.Lib.Repo


update :: VisM ()
update = do
  cfg <- ask
  --lift $ print cfg
  update_repos
  return ()

update_repos :: VisM ()
update_repos = do
  mgh_user <- getConfig "github_username" ""
  case mgh_user of 
    "" -> return ()
    gh_user -> do
       repos <- getConfig "localrepos" []
       --lift $ print repos
       forM_ repos $ \repo -> do    
          case H.lookup "name" repo of
            Just repo_name -> lift $ has_github_repo gh_user repo_name
            Nothing -> return ()


-- check packages
-- checkout repos
-- set/check path

has_pkgs pkgs = do
  forM_ pkgs $ \pkg-> do
     system $ "sudo apt-get install -y -q "++pkg 

has_github_repo user reponame = do
  gotoHomeDir
  e <- doesDirectoryExist reponame
  when (not e) $ do 
    system $ "git clone git@github.com:"++user++"/"++reponame++".git"
    return ()

