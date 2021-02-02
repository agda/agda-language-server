{-# LANGUAGE OverloadedStrings #-}

module Process where

import Control.Exception ( try, SomeException )
import Data.Maybe (isJust)
import System.IO (hClose, hPutStr)
import System.Process
import GHC.IO.Handle (Handle)

import Agda.VersionCommit (versionWithCommitInfo)

getAgdaVersion :: String
getAgdaVersion = versionWithCommitInfo

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