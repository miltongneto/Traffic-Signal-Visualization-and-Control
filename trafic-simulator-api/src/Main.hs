{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty
import Config.Routes
import Database
import Jobs

myCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = "OPTIONS" : simpleMethods }

main = do
  putStrLn "Starting Server..."
  scotty 3003 $ do 
    middleware myCors
    routes

startJob = do
  jobs <- new
  register jobs "updateTrafficSignalStatus" "1s" updateTrafficSignalStatus
  changeInterval jobs 1
  startExec jobs
