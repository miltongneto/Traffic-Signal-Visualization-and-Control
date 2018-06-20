{-# LANGUAGE DeriveGeneric #-}
module Model.TrafficSignal where 

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Time

data TrafficSignal = TrafficSignal { 
  trafficId            :: Int,
  localizacao1        :: String,
  localizacao2        :: String,
  funcionamento       :: String,
  utilizacao          :: String,
  sinalSonoro         :: String,
  sinalizadorCiclista :: String,
  latitude            :: Double,
  longitude           :: Double,
  status              :: Int,
  lastUpdate          :: LocalTime
} deriving (Show, Generic)

instance ToJSON TrafficSignal
instance FromJSON TrafficSignal