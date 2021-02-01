{-# LANGUAGE OverloadedStrings #-}

module Process where

-- import Control.Concurrent.STM (atomically)

-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Exception
import Control.Exception (throwIO)
import Data.Maybe (isJust)
import System.IO (hClose, hPutStr)
import System.Process
import GHC.IO.Handle (Handle)

-- import System.Process.Typed
--


getAgdaVersion :: FilePath -> IO (Maybe String)
getAgdaVersion path = do
  result <- try (readProcess path ["-V"] "") :: IO (Either SomeException String)
  case result of
    Left _ -> return Nothing
    Right raw -> case splitAt 13 raw of
      ("Agda version ", version) -> return $ Just $ init version
      _ -> return Nothing

callAgda :: FilePath -> IO (Maybe (Handle, Handle))
callAgda path = do
  -- (hin, hout, herr, handle) <-
  result <- try $ createProcess
      (proc path ["--interaction"])
        { std_in = CreatePipe,
          std_out = CreatePipe
        } :: IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
        
  case result of
    Right (Just hin, Just hout, _, _) -> return $ Just (hin, hout)
    _ -> return Nothing