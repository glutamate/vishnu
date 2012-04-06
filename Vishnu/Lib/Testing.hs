module Vishnu.Lib.Testing where

import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Directory
import Data.List
import Data.Ord

import Vishnu.Lib.Common
import Vishnu.Lib.Repo
 
test :: VisM ()
test = perRepoFromArgs $ \repo -> do
    liftIO $ gotoHomeSubDir repo
    ex <- liftIO $ doesDirectoryExist "tests"
    when ex $ runTestsIn repo

runTestsIn repo = do 
   liftIO $ gotoHomeSubDir $ repo++"/tests"
   files <- liftIO $ getDirectoryContents "."
   liftIO$ putStrLn $ "Repo: "++repo
   mapM_ dispatchTest files

dispatchTest file | ".bug" `isSuffixOf` file = bugTest file
                  | otherwise = return ()

bugTest file = do
   liftIO $ putStrLn $ " file "++file
   out <- sh $ "bays --test "++file
   liftIO $ putStrLn $ unlines $ map ("   "++) $ lines out
   return ()