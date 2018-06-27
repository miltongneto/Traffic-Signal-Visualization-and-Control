{-# LANGUAGE OverloadedStrings #-}
module Config.Routes where

import Web.Scotty
import Controller.TrafficSignalController as TSC

routes :: ScottyM ()
routes = do 
  get "/api/traffic-signal" TSC.getAll
  get "/api/traffic-signal/:id" TSC.getById
  post "/api/traffic-signal/:id" TSC.updateById
  post "/api/traffic-signal/forceStatusChange/:id" TSC.forceStatusChange