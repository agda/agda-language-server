{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (when)
import Options
import Server (run)
-- import Simple (run)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.Environment
import System.FilePath (takeDirectory, (</>))
import System.IO
import Text.Read (readMaybe)

#if MIN_VERSION_Agda(2,8,0)
import Agda.Setup (setup)
#endif

main :: IO ()
main = do
  -- set locale to UTF-8
  -- https://github.com/agda/agda-language-server/issues/24
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  hSetEncoding stderr utf8

-- getExecutablePath returns argv[0] in WASM, which is useless
#ifndef wasm32_HOST_ARCH
  -- A release bundle ships a "data" directory next to the executable
  -- (not inside a directory named after it). If that sibling directory
  -- exists, point $Agda_datadir at it instead of Agda's compiled-in
  -- builder path.
  executablePath <- getExecutablePath
  let dataDir = takeDirectory executablePath </> "data"
  hasBundledDataDir <- doesDirectoryExist dataDir
  when hasBundledDataDir $ do
    setEnv "Agda_datadir" dataDir
#endif

  options <- getOptionsFromArgv
  case () of
    _ | optHelp options -> putStrLn usageMessage
      | optVersion options -> putStrLn versionString
#if MIN_VERSION_Agda(2,8,0)
      | optSetup options -> do
          setup True
          return ()
#endif
      | otherwise -> do
          _ <- run options
          -- _ <- run
          return ()
