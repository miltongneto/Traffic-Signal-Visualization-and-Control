{-# LANGUAGE OverloadedStrings #-}
module Controller.TrafficSignalController where

import Web.Scotty
import Model.TrafficSignal
import Database

t1 = TrafficSignal { 
  trafficId = 1, 
  localizacao1 = "loc1", 
  localizacao2 = "loc2",
  funcionamento = "func",
  utilizacao = "ut",
  sinalSonoro = "S",
  sinalizadorCiclista = "T",
  latitude = -8.4235,
  longitude = -54.1232
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
  longitude = -34.1232
}
allSignal = [t1, t2]

getById :: ActionM ()
getById = do 
  id <- param "id"
  json $ filter (equalId id) allSignal

equalId :: Int -> TrafficSignal -> Bool
equalId id trafficSignal = trafficId trafficSignal == id


getAll :: ActionM ()
getAll = do 
  json allSignal

updateById :: ActionM ()
updateById = do 
  text "update by id"