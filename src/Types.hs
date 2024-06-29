module Types
    ( Attributes(..)
    , Outfit(..)
    , Engine(..)
    , Weapon(..)
    , Ship(..)
    , ESData(..)
    , emptyData
    ) where

import Data.Aeson
import GHC.Generics

data Engine = Engine { power :: Double
                     , engineEnergy :: Double
                     , engineHeat :: Double
                     }
  deriving (Show, Generic)

data Weapon = Weapon { reload :: Double
                     , firingEnergy :: Double
                     , firingHeat :: Double }
  deriving (Show, Generic)

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
                             , cooling :: Double
                             , coolingInefficiency :: Int
                             , energyCapacity :: Int
                             , energyGeneration :: Double
                             , energyConsumption :: Double
                             , heatGeneration :: Double
                             , shieldEnergy :: Double
                             , shieldHeat :: Double
                             , hullEnergy :: Double
                             , hullHeat :: Double
                             , licenses :: [String]
                             }
  deriving (Show, Generic)

data Outfit = Outfit { attributes :: Attributes
                     , thrustData :: Maybe Engine
                     , turnData :: Maybe Engine
                     , reverseData :: Maybe Engine
                     , weaponData :: Maybe Weapon
                     }
  deriving (Show, Generic)

data Ship = Ship { attributes :: Attributes
                 , drag :: Double
                 , heatDissipation :: Double
                 }
  deriving (Show, Generic)

data ESData = ESData { ships :: [Ship]
                     , outfits :: [Outfit]
                     }
  deriving (Show, Generic)

instance ToJSON Engine where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Weapon where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Attributes where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Outfit where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Ship where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON ESData where
    toEncoding = genericToEncoding defaultOptions

emptyData :: ESData
emptyData = ESData [] []
