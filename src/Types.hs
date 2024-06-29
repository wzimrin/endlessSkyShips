module Types
    ( Attributes(..)
    , Outfit(..)
    , Engine(..)
    , Weapon(..)
    , Ship(..)
    ) where

data Engine = Engine { power :: Double
                     , engineEnergy :: Double
                     , engineHeat :: Double
                     }
  deriving Show

data Weapon = Weapon { reload :: Int
                     , firingEnergy :: Double
                     , firingHeat :: Double }
  deriving Show

data Attributes = Attributes { name :: String
                             , category :: String
                             , culture :: String
                             , cost :: Int
                             , mass :: Double
                             , outfitSpace :: Int
                             , cargoSpace :: Int
                             , weaponCapacity :: Int
                             , engineCapacity :: Int
                             , fuelCapacity :: Int
                             , ramscoop :: Double
                             , bunks :: Int
                             , requiredCrew :: Int
                             , cooling :: Int
                             , coolingInefficiency :: Int
                             , energyCapacity :: Int
                             , energyGeneration :: Double
                             , energyConsumption :: Double
                             , heatGeneration :: Double
                             , shieldEnergy :: Double
                             , shieldHeat :: Double
                             , hullEnergy :: Double
                             , hullHeat :: Double
                             }
  deriving Show

data Outfit = Outfit { attributes :: Attributes
                     , thrustData :: Maybe Engine
                     , turnData :: Maybe Engine
                     , reverseData :: Maybe Engine
                     , weaponData :: Maybe Weapon
                     }
  deriving Show

data Ship = Ship { attributes :: Attributes
                 , drag :: Double
                 , heatDissipation :: Double
                 }
  deriving Show
