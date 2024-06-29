module Main (main) where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as ByteString
import Data.List (isSuffixOf)
import Control.Monad
import System.Directory

import Parsing
import Processing;
import Types
import qualified Debug

outfitPath :: String
outfitPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/outfits.txt"

weaponPath :: String
weaponPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/weapons.txt"

enginePath :: String
enginePath = "/Applications/Endless Sky.app/Contents/Resources/data/human/engines.txt"

shipPath :: String
shipPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/ships.txt"

parseAndProcess :: String -> String -> IO ESData
parseAndProcess culture path = do
  Debug.log $ "parsing: " ++ path
  processFile culture <$> parseFile path

basePath :: String
basePath = "/Applications/Endless Sky.app/Contents/Resources/data/"

showData :: String -> ESData -> IO ()
showData name ESData { ships, outfits } = do
  putStrLn $ "ships for: " ++ name
  mapM_ print $ take 5 ships
  putStrLn $ "outfits for: " ++ name
  mapM_ print $ take 5 outfits

showFile :: String -> String -> IO ()
showFile culture path = parseAndProcess culture path >>= showData path

listMatching :: (String -> IO Bool) -> String -> IO [String]
listMatching f path = do
  contents <- listDirectory path
  filterM (f . (path ++)) contents

listDirectories :: String -> IO [String]
listDirectories = listMatching doesDirectoryExist

listFiles :: String -> IO [String]
listFiles = listMatching isTextFile
 where isTextFile file' = do
         exists <- doesFileExist file'
         return $ exists && ".txt" `isSuffixOf` file'

getESDataForSubfolder :: String -> String -> IO ESData
getESDataForSubfolder path subfolder = do
  files <- listFiles (path ++ subfolder ++ "/")
  Debug.log $ "found files: " ++ show files ++ " " ++ show (path ++ subfolder)
  mergeFiles <$> mapM (parseAndProcess subfolder . ((basePath ++ subfolder ++ "/") ++)) files

getESData :: String -> IO ESData
getESData path = mergeFiles <$> (listDirectories path >>= mapM (getESDataForSubfolder path))

main :: IO ()
main = do
  getESData basePath >>= ByteString.writeFile "esdata.json" . encodePretty
  putStrLn "Wrote esdata.json"
