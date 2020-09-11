module Main where

import Application
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  putStrLn "Running at: http://localhost:3000"
  run 3000 application
