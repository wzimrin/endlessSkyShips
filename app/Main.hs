module Main (main) where

import Text.Parsec.String
import Parsing
import Processing;
import Types

outfitPath :: String
outfitPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/outfits.txt"

weaponPath :: String
weaponPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/weapons.txt"

enginePath :: String
enginePath = "/Applications/Endless Sky.app/Contents/Resources/data/human/engines.txt"

shipPath :: String
shipPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/ships.txt"

parseAndProcess :: String -> String -> IO ([Ship], [Outfit])
parseAndProcess culture path = processFile culture <$> parseFile path

showFile :: String -> String -> IO ()
showFile culture path = do
  (ships, outfits) <- parseAndProcess culture path
  putStrLn $ "ships for: " ++ path
  mapM_ print $ take 5 ships
  putStrLn $ "outfits for: " ++ path
  mapM_ print $ take 5 outfits

main :: IO ()
main = do
  showFile "human" outfitPath
  showFile "human" weaponPath
  showFile "human" enginePath
  showFile "human" shipPath
