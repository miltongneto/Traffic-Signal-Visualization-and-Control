module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty
import Config.Routes

main = do
  putStrLn "Starting Server..."
  scotty 3003 $ do 
    middleware simpleCors
    routes