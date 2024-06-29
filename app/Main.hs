module Main (main) where

import Control.Monad
import Data.Functor
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String

data RawBlock = RawBlock String [String] [RawBlock]

data ESBlock = ESBlock [String] ESBlocks
  deriving Show

type ESBlocks = Map.Map String [ESBlock]

data Engine = Engine { power :: Double
                     , engineEnergy :: Double
                     , engineHeat :: Double
                     }
  deriving Show

data Weapon = Weapon { reload :: Int
                     , weaponEnergy :: Double
                     , weaponHeat :: Double }
  deriving Show

data Outfit = Outfit { name :: String
                     , category :: String
                     , cost :: Int
                     , mass :: Int
                     , outfitSpace :: Int
                     , weaponCapacity :: Int
                     , engineCapacity :: Int
                     , cooling :: Int
                     , coolingInefficiency :: Int
                     , energyCapacity :: Int
                     , energyGeneration :: Double
                     , energyConsumption :: Double
                     , shieldEnergy :: Double
                     , shieldHeat :: Double
                     , heatGeneration :: Double
                     , thrustData :: Maybe Engine
                     , turnData :: Maybe Engine
                     , reverseData :: Maybe Engine
                     , weaponData :: Maybe Weapon
                     }
  deriving Show

data Ship = Ship { shipName :: String
                 , shipCategory :: String
                 , shipCost :: Int
                 , shipMass :: Int
                 , drag :: Double
                 , heatDissipation :: Double
                 , shipCargoSpace :: Int
                 , shipOutfitSpace :: Int
                 , shipWeaponCapacty :: Int
                 , shipEngineCapacity :: Int
                 }


convertBlocks :: [RawBlock] -> ESBlocks
convertBlocks = Map.fromListWith (++) . map blockToPair
  where
    blockToPair (RawBlock name' vals children) = (name', [ESBlock vals (convertBlocks children)])

getValue :: String -> String -> ESBlocks -> String
getValue backup key children = maybe backup firstVal (Map.lookup key children)
  where
    firstVal [] = backup
    firstVal ((ESBlock [] _):_) = backup
    firstVal ((ESBlock (val:_) _):_) = val

getChildren :: String -> ESBlocks -> ESBlocks
getChildren key children = maybe Map.empty firstChildren (Map.lookup key children)
  where
    firstChildren [] = Map.empty
    firstChildren ((ESBlock _ inner):_) = inner

getStringValue :: String -> ESBlocks -> String
getStringValue = getValue ""

getReadValue :: Read t => String -> ESBlocks -> t
getReadValue key = read . getValue "0" key

getEngine :: String -> String -> String -> ESBlocks -> Maybe Engine
getEngine thrust energy heat children =
  if power' == 0 || energy' == 0 || heat' == 0
  then Nothing
  else Just (Engine
             { power = power'
             , engineEnergy = energy'
             , engineHeat = heat'
             })
  where
    power' = getReadValue thrust children
    energy' = getReadValue energy children
    heat' = getReadValue heat children

getWeapon ::  ESBlocks -> Maybe Weapon
getWeapon children =
  if reload' == 0 || energy' == 0 || heat' == 0
  then Nothing
  else Just (Weapon
             { reload = reload'
             , weaponEnergy = energy'
             , weaponHeat = heat'
             })
  where
    weapon = getChildren "weapon" children
    reload' = getReadValue "reload" weapon
    energy' = getReadValue "firing energy" weapon
    heat' = getReadValue "firing heat" weapon

parseOutfit :: ESBlock -> Outfit
parseOutfit (ESBlock [] _) = error "Outfit missing name"
parseOutfit (ESBlock (name':_) children) = Outfit
  { name = name'
  , category = getStringValue "category" children
  , cost = getReadValue "cost" children
  , mass = getReadValue "mass" children
  , outfitSpace = getReadValue "outfit space" children
  , weaponCapacity = getReadValue "weapon capacity" children
  , engineCapacity = getReadValue "engine capacity" children
  , cooling = getReadValue "cooling" children
  , coolingInefficiency = getReadValue "cooling inefficiency" children
  , energyCapacity = getReadValue "energy capacity" children
  , energyGeneration = getReadValue "energy generation" children
  , energyConsumption = getReadValue "energy consumption" children
  , shieldEnergy = getReadValue "shield energy" children
  , shieldHeat = getReadValue "shield heat" children
  , heatGeneration = getReadValue "heat generation" children
  , thrustData = getEngine "thrust" "thrusting energy" "thrusting heat" children
  , turnData = getEngine "turn" "turning energy" "turning heat" children
  , reverseData = getEngine "reverse thrust" "reverse thrusting energy" "reverse thrusting heat" children
  , weaponData = getWeapon children
  }

comment :: Parser [t]
comment = do
  _ <- string "#"
  _ <- manyTill anyChar (try endOfLine)
  return []

single :: Parser String
single = many1 (alphaNum <|> oneOf "-.")

quoted :: Parser String
quoted = between (string "\"") (string "\"") $ many1 $ noneOf "\""

backquoted :: Parser String
backquoted = between (string "`") (string "`") $ many1 $ noneOf "`"

item :: Parser String
item = single <|> quoted <|> backquoted

nextLine :: Parser ()
nextLine = skipMany1 (skipMany (string " ") *> endOfLine)

indent :: Int -> Parser ()
indent n = void $ count n (string " " <|> string "\t")

gap :: Parser ()
gap = skipMany1 (string " ")

line :: Int -> Parser [String]
line n = do
  indent n
  result <- sepBy1 item gap
  nextLine
  return result

rawBlock :: Int -> Parser RawBlock
rawBlock n = do
  (blockName : vals) <- line n
  children <- many (try (rawBlock (n+1)))
  return $ RawBlock blockName vals children

file :: Parser ESBlocks
file = convertBlocks . join <$> many (comment <|> (nextLine $> []) <|> fmap pure (rawBlock 0))

outfitPath :: String
outfitPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/outfits.txt"

weaponPath :: String
weaponPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/weapons.txt"

enginePath :: String
enginePath = "/Applications/Endless Sky.app/Contents/Resources/data/human/engines.txt"

shipPath :: String
shipPath = "/Applications/Endless Sky.app/Contents/Resources/data/human/ships.txt"

showOutfit :: String -> IO ()
showOutfit path = do
  parsed <- parseFromFile file path
  let outfit = last $ either (const []) (Map.findWithDefault [] "outfit") parsed
  print $ parseOutfit outfit

showFirst :: String -> IO ()
showFirst path = do
  parsed <- parseFromFile file path
  let (key, vals) = last $ either (const []) Map.toList parsed
  putStrLn key
  print $ last vals

main :: IO ()
main = do
  showOutfit outfitPath
  showOutfit weaponPath
  showOutfit enginePath
  showFirst shipPath
