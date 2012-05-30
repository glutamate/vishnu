module Vishnu.Lib.Common where

import Control.Monad.Reader
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import Data.Maybe 

import System.Process
import System.Exit
import System.IO
import Data.List



data VisS = VisS { configVal :: Value, 
                   moreArgs :: [String],
                   optArgs :: [String] }

type VisM = ReaderT VisS IO

runVisM :: [String] -> VisM a -> IO a
runVisM args vm = do
  gotoHomeDir
  let (margs, optargs) = partition ((/='-') . head) args
  e <- doesFileExist ".vishnu.json"
  if not e 
     then runReaderT vm $ VisS Null margs optargs
     else do mv <- fmap decode' $ B.readFile ".vishnu.json"
             case mv of 
                Nothing -> fail "vishnu: error reading config file .vishnu.json"
                Just v -> runReaderT vm $ VisS v margs optargs

getConfig :: FromJSON a => String -> a-> VisM a
getConfig s def = do
  VisS val _ _ <- ask
  return $ getConfig' s val def

getConfig' [] val def = case fromJSON val of
                          Success x -> x
                          _ -> def
getConfig' s (Object obj) def = 
    let (nm,rest) = span (/='.') s
    in case H.lookup (T.pack nm) obj of
         Nothing -> def
         Just v -> getConfig' (drop 1 rest) v def
getConfig' s v def = def



gotoHomeSubDir s = do
  home <- getHomeDirectory
  setCurrentDirectory $ home++"/"++s

gotoHomeDir = do
  home <- getHomeDirectory
  setCurrentDirectory $ home

sh :: String -> VisM String
sh cmd = liftIO $ do (hin, hout, herr, ph) <- runInteractiveCommand cmd
                     excode <-  waitForProcess ph
                     sout <-  hGetContents hout
                     serr <- hGetContents herr
                     case excode of
                        ExitSuccess -> return sout
                        ExitFailure n ->
                                return $ concat ["process error ",
                                           show n,
                                           " :",
                                           serr
                                          ]

perRepoFromArgs ma = do
  args <- fmap moreArgs ask
--  liftIO $ print args
  case args of 
      [] -> perRepo ma
      repos -> do localRepos <- getLocalRepos
                  forM_  localRepos $ \lr-> 
                     when (any (`isPrefixOf` lr) repos) $ ma lr >> return ()


perRepo ma = do
  repos <- getLocalRepos
  forM_ repos ma

getLocalRepos =  do
    repos <- getConfig "localrepos" [] -- :: [H.HashMap String String] 
    return $  catMaybes $ flip map repos $ \repo ->  H.lookup "name" repo

reposDependOn nm = do
    repos <- getConfig "localrepos" [] -- :: [H.HashMap String String] 
    return $  catMaybes $ flip map repos $ \repo ->  
               case (H.lookup "name" repo, H.lookup "depends" repo) of
                  (Just rname, Just depends) | nm `elem` words depends
                                                 -> Just rname
                                             | otherwise
                                                 -> Nothing
                                                   
                  _ -> Nothing

                   
