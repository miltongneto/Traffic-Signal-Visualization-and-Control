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
  id <- param "id"
  trafficSignal <- jsonData
  liftIO $ updateTrafficSignalById id trafficSignal

forceStatusChange :: ActionM ()
forceStatusChange = do 
  id <- param "id"
  liftIO $ forceChangeStatus id