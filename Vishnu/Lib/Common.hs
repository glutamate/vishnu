module Vishnu.Lib.Common where

import Control.Monad.Reader
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T

import System.Process
import System.Exit
import System.IO


data VisS = VisS { configVal :: Value }

type VisM = ReaderT VisS IO

runVisM :: VisM a -> IO a
runVisM vm = do
  gotoHomeDir
  e <- doesFileExist ".vishnu.json"
  if not e 
     then runReaderT vm $ VisS Null
     else do mv <- fmap decode' $ B.readFile ".vishnu.json"
             case mv of 
                Nothing -> fail "vishnu: error reading config file .vishnu.json"
                Just v -> runReaderT vm $ VisS v

getConfig :: FromJSON a => String -> a-> VisM a
getConfig s def = do
  VisS val <- ask
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
