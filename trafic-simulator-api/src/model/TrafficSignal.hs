{-# LANGUAGE DeriveGeneric #-}
module Model.TrafficSignal where 

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data TrafficSignal = TrafficSignal { 
  trafficId            :: Int,
  localizacao1        :: String,
  localizacao2        :: String,
  funcionamento       :: String,
  utilizacao          :: String,
  sinalSonoro         :: Bool,
  sinalizadorCiclista :: Bool,
  latitude            :: Float,
  longitude           :: Float
} deriving (Show, Generic)

instance ToJSON TrafficSignal
instance FromJSON TrafficSignal