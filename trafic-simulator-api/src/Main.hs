{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty
import Config.Routes
import Database
import Jobs

main = do
  putStrLn "Starting Server..."
  scotty 3003 $ do 
    middleware simpleCors
    routes

startJob = do
  jobs <- new
  register jobs "updateTrafficSignalStatus" "1s" updateTrafficSignalStatus
  changeInterval jobs 1
  startExec jobs
