{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Switchboard (Switchboard, new, setupLanguageContextEnv, destroy, agdaCustomMethod) where

import qualified Agda
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.IORef
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.IO as Text
import Language.LSP.Protocol.Message
import Language.LSP.Server
import Monad
import Options (Config)
import qualified Server.ResponseController as ResponseController
import System.IO (stderr)

data Switchboard = Switchboard
  { sbPrintLog :: ThreadId,
    sbSendResponse :: ThreadId,
    sbRunAgda :: ThreadId,
    sbLanguageContextEnv :: IORef (Maybe (LanguageContextEnv Config))
  }

-- | All channels go in and out from here
new :: Env -> IO Switchboard
new env = do
  ctxEnvIORef <- newIORef Nothing
  Switchboard
    <$> forkIO (keepPrintingLog env)
    <*> forkIO (keepSendindResponse env ctxEnvIORef)
    <*> forkIO (runReaderT Agda.start env)
    <*> pure ctxEnvIORef

-- | For sending reactions to the client
setupLanguageContextEnv :: Switchboard -> LanguageContextEnv Config -> IO ()
setupLanguageContextEnv switchboard ctxEnv = do
  writeIORef (sbLanguageContextEnv switchboard) (Just ctxEnv)

destroy :: Switchboard -> IO ()
destroy switchboard = do
  killThread (sbPrintLog switchboard)
  killThread (sbSendResponse switchboard)
  killThread (sbRunAgda switchboard)
  writeIORef (sbLanguageContextEnv switchboard) Nothing

-- | Keep printing log to stderr
-- Consumer of `envLogChan`
keepPrintingLog :: Env -> IO ()
keepPrintingLog env = forever $ do
  result <- readChan (envLogChan env)
  when (envDevMode env) $ do
    Text.hPutStrLn stderr result

-- | Keep sending reactions
-- Consumer of `envResponseChan`
keepSendindResponse :: Env -> IORef (Maybe (LanguageContextEnv Config)) -> IO ()
keepSendindResponse env ctxEnvIORef = forever $ do
  response <- readChan (envResponseChan env)

  result <- readIORef ctxEnvIORef
  forM_ result $ \ctxEnv -> do
    runLspT ctxEnv $ do
      callback <- liftIO $ ResponseController.dispatch (envResponseController env)

      let value = JSON.toJSON response
      sendRequest agdaCustomMethod value $ \_result -> liftIO $ do
        -- writeChan (envLogChan env) $ "[Response] >>>> " <> pack (show value)
        callback ()

agdaCustomMethod :: SMethod ('Method_CustomMethod "agda")
agdaCustomMethod = SMethod_CustomMethod (Proxy @"agda")
