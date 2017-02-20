module Main where

import Server (runServer)

main :: IO ()
main = do 
  putStrLn $ "Up and running"
  runServer
