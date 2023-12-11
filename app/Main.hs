module Main where

import Options
import Server (run)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = do
  -- set locale to UTF-8
  -- https://github.com/agda/agda-language-server/issues/24
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  options <- getOptionsFromArgv
  if optHelp options
    then putStrLn usageMessage
    else do
      _ <- run options
      return ()
