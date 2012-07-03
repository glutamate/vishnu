module Vishnu.Cmds.Update where

import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.Cmd
import Control.Monad.Reader
import Data.List
import System.Exit
import qualified Data.HashMap.Lazy as H
import Data.Aeson.Types
import qualified  Data.Text as T
import qualified  Data.Vector as V

import Vishnu.Lib.Common
import Vishnu.Lib.Repo


update :: VisM ()
update = do
  cfg <- ask
  --lift $ print cfg
  update_pkgs
  update_repos
  update_path
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

update_pkgs = do
  pkgs <- fmap mergePkgs $ getConfig "ubuntu_pkgs" Null
  lift $ has_pkgs pkgs

update_path = do
  mpath <- getConfig "bin_path" ""
  --lift $ print mpath
  case mpath of 
     "" -> return ()
     path -> do  
       --does .profile have vishnu path?
       lift $ gotoHomeDir
       --lift $ print path
       prof_lines <- fmap lines $ lift $ readFile ".profile"
       case filter ("VISHNUPATH="`isPrefixOf`) prof_lines of
         [] -> do let newvpath = "echo \"VISHNUPATH=\\\""++path ++"\\\"\" >> .profile"
                  lift $ putStrLn $ newvpath
                  lift $ system $ newvpath
                  return ()
         _ -> return () -- FIXME what if needs editing
       let addpth = "PATH=\"$VISHNUPATH:$PATH\""
       when (not $ any (==addpth) prof_lines) $ do
          let news = "echo \"PATH=\\\"\\$VISHNUPATH:\\$PATH\\\"\" >> .profile"
          --lift $ putStrLn news
          lift $ system $ news
          return ()
       
       --does .profile add vishnu path to path?
       

mergePkgs (String txt) = words $ T.unpack txt
mergePkgs (Array arr) = concat $ map mergePkgs $ V.toList arr
mergePkgs v = []

has_pkgs [] = return ()
has_pkgs pkgs = do
     system $ "sudo apt-get install -y -q "++unwords pkgs
     return ()
has_github_repo user reponame = do
  gotoHomeDir
  e <- doesDirectoryExist reponame
  when (not e) $ do 
    system $ "git clone git@github.com:"++user++"/"++reponame++".git"
    return ()

