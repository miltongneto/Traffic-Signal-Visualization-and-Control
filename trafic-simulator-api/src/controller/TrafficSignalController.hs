{-# LANGUAGE OverloadedStrings #-}
module Controller.TrafficSignalController where

import Web.Scotty
import Web.Scotty.Internal.Types
import Model.TrafficSignal
import Database
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson.Types
import Model.TrafficSignal (status)
import Data.Time

--getDate = getCurrentTime >>= return . toGregorian . utctDay

getTime = TimeOfDay 10 30 30
getDay = fromGregorian 2018 6 15 

getData = LocalTime getDay getTime

t1 = TrafficSignal { 
  trafficId = 1, 
  localizacao1 = "loc1", 
  localizacao2 = "loc2",
  funcionamento = "func",
  utilizacao = "ut",
  sinalSonoro = "S",
  sinalizadorCiclista = "T",
  latitude = -8.4235,
  longitude = -54.1232,
  Model.TrafficSignal.status = 1,
  lastUpdate = getData
}
t2 = TrafficSignal {
  trafficId = 2, 
  localizacao1 = "loc3", 
  localizacao2 = "loc4",
  funcionamento = "func",
  utilizacao = "utl",
  sinalSonoro = "N",
  sinalizadorCiclista = "N",
  latitude = -12.4235,
  longitude = -34.1232,
  Model.TrafficSignal.status = 2,
  lastUpdate = getData 
}
allSignal = [t1, t2]

getById :: ActionM ()
getById = do
  id <- param "id"
  trafficSignal <- liftIO $ getTrafficSignalById id
  json trafficSignal

getAll :: ActionM ()
getAll = do 
  trafficSignals <- liftIO $ getAllTrafficSignals
  json trafficSignals

updateById :: ActionM ()
updateById = do 
  text "update by id"