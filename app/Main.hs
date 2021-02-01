module Main where

import Server (run)

main :: IO ()
main = do 
  _ <- run
  return ()
