module Main where

import Web.Scotty
import Config.Routes

main = do
  putStrLn "Starting Server..."
  scotty 3003 routes