module Main where

import Control.Monad (when)
import Options
import Server (run)
-- import Simple (run)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.Environment
import System.FilePath ((</>))
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = do
  -- set locale to UTF-8
  -- https://github.com/agda/agda-language-server/issues/24
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  hSetEncoding stderr utf8

  -- The GitHub CI-built executable lacks the correct data directory path.
  -- If there's directory named "data" in the executable's directory,
  -- then we assume that the executable is built by GitHub CI
  -- and we should set the $Agda_datadir environment variable to the correct directory.
  executablePath <- getExecutablePath
  let dataDir = executablePath </> "data"
  isBuiltByCI <- doesDirectoryExist dataDir
  when isBuiltByCI $ do
    setEnv "Agda_datadir" dataDir

  options <- getOptionsFromArgv
  if optHelp options
    then putStrLn usageMessage
    else do
      _ <- run options
      -- _ <- run
      return ()
