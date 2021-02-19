{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Agda.Interaction.Response as Agda
import Agda.Interaction.Base (IOTCM)
import Agda.TypeChecking.Monad (TCMT)
import Control.Concurrent
import Control.Monad.Reader
import Control.Concurrent.Foreman (Foreman)
import qualified Control.Concurrent.Foreman as Foreman
import Control.Throttler (Throttler)
import qualified Control.Throttler as Throttler
import Data.IORef
import Data.Text (Text)
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data Ntf = NtfResponse String | NtfDummy | NtfResponseEnd
  deriving (Generic)

instance ToJSON Ntf

--------------------------------------------------------------------------------

data Env = Env
  { envLogChan :: Chan Text,
    envCmdThrottler :: Throttler IOTCM,
    envResponseChan :: Chan Ntf,
    envResponseController :: Foreman,
    envDevMode :: Bool
  }

type ServerM' m = ReaderT Env m

type ServerM = ServerM' IO

createInitEnv :: Bool -> IO Env
createInitEnv devMode =
  Env <$> newChan
    <*> Throttler.new
    <*> newChan
    <*> Foreman.new
    <*> pure devMode

runServerLSP :: Env -> LanguageContextEnv () -> LspT () (ServerM' m) a -> m a
runServerLSP env ctxEnv program = runReaderT (runLspT ctxEnv program) env

writeLog :: (Monad m, MonadIO m) => Text -> ServerM' m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

-- | Provider
provideCommand :: (Monad m, MonadIO m) => IOTCM -> ServerM' m ()
provideCommand iotcm = do
  throttler <- asks envCmdThrottler
  liftIO $ Throttler.put throttler iotcm

-- | Consumter
consumeCommand :: (Monad m, MonadIO m) => Env -> m IOTCM
consumeCommand env = liftIO $ Throttler.take (envCmdThrottler env)

waitUntilResponsesSent :: (Monad m, MonadIO m) => ServerM' m ()
waitUntilResponsesSent = do
  foreman <- asks envResponseController
  liftIO $ Foreman.setGoalAndWait foreman 

signalCommandFinish :: (Monad m, MonadIO m) => ServerM' m ()
signalCommandFinish = do
  writeLog "[Command] Finished"
  -- send `NtfResponseEnd`
  env <- ask
  liftIO $ writeChan (envResponseChan env) NtfResponseEnd
  -- allow the next Command to be consumed
  throttler <- asks envCmdThrottler
  liftIO $ Throttler.move throttler

sendResponse :: (Monad m, MonadIO m) => Env -> (Agda.Response, String) -> TCMT m ()
sendResponse env (_response, lispified) = do
  liftIO $ writeChan (envResponseChan env) (NtfResponse lispified)
