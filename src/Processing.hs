module Processing ( processShip
                  , processOutfit
                  , processFile
                  , joinFiles
                  ) where

import qualified Data.Map as Map
import Data.Maybe
import Types
import Parsing

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
getReadValue key blocks = read value
  where
    value = case getValue "0" key blocks of
      ('.':rest) -> "0." ++ rest
      value' -> value'

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
             , firingEnergy = energy'
             , firingHeat = heat'
             })
  where
    weapon = getChildren "weapon" children
    reload' = getReadValue "reload" weapon
    energy' = getReadValue "firing energy" weapon
    heat' = getReadValue "firing heat" weapon

processAttributes :: String -> String -> ESBlocks -> Attributes
processAttributes name culture children = Attributes
  { name
  , culture
  , category = getStringValue "category" children
  , cost = getReadValue "cost" children
  , mass = getReadValue "mass" children
  , outfitSpace = getReadValue "outfit space" children
  , cargoSpace = getReadValue "cargo space" children
  , weaponCapacity = getReadValue "weapon capacity" children
  , engineCapacity = getReadValue "engine capacity" children
  , fuelCapacity = getReadValue "fuel capacity" children
  , ramscoop = getReadValue "ramscoop" children
  , bunks = getReadValue "bunks" children
  , requiredCrew = getReadValue "required crew" children
  , cooling = getReadValue "cooling" children
  , coolingInefficiency = getReadValue "cooling inefficiency" children
  , energyCapacity = getReadValue "energy capacity" children
  , energyGeneration = getReadValue "energy generation" children
  , energyConsumption = getReadValue "energy consumption" children
  , heatGeneration = getReadValue "heat generation" children
  , shieldEnergy = getReadValue "shield energy" children
  , shieldHeat = getReadValue "shield heat" children
  , hullEnergy = getReadValue "hull energy" children
  , hullHeat = getReadValue "hull heat" children
  }

processOutfit :: String -> ESBlock -> Outfit
processOutfit _ s@(ESBlock [] _) = error $ "Outfit missing name " ++ show s
processOutfit culture (ESBlock (name:_) children) = Outfit
  { attributes = processAttributes name culture children
  , thrustData = getEngine "thrust" "thrusting energy" "thrusting heat" children
  , turnData = getEngine "turn" "turning energy" "turning heat" children
  , reverseData = getEngine "reverse thrust" "reverse thrusting energy" "reverse thrusting heat" children
  , weaponData = getWeapon children
  }

processShip :: String -> ESBlock -> Maybe Ship
processShip _ s@(ESBlock [] _) = error $ "Ship missing name " ++ show s
processShip culture (ESBlock [name] children) = Just $ Ship
  { attributes = processAttributes name culture shipAttributes
  , drag = getReadValue "drag" shipAttributes
  , heatDissipation = getReadValue "heat dissipation" shipAttributes
  }
  where
    shipAttributes = getChildren "attributes" children
processShip _ (ESBlock _ _) = Nothing

processFile :: String -> ESBlocks -> ESData
processFile culture blocks = ESData {ships, outfits}
  where
    ships = mapMaybe (processShip culture) $ Map.findWithDefault [] "ship" blocks
    outfits = map (processOutfit culture) $ Map.findWithDefault [] "outfit" blocks

joinFiles :: ESData -> ESData -> ESData
joinFiles esdata esdata' = ESData { ships = esdata.ships ++ esdata'.ships
                                  , outfits = esdata.outfits ++ esdata'.outfits
                                  }
