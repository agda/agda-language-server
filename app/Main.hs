module Main where

import           Options
import           Server                         ( run )
import           System.Console.GetOpt
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
  options <- getOptions
  if optHelp options
    then putStrLn usageMessage
    else do
      _ <- run options
      return ()
