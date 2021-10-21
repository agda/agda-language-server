{-# LANGUAGE FlexibleContexts #-}
module Monad where

import           Agda.IR

import           Agda.Interaction.Base          ( IOTCM )
import           Agda.TypeChecking.Monad        ( TCMT )
import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Server.CommandController       ( CommandController )
import qualified Server.CommandController      as CommandController
import           Server.ResponseController      ( ResponseController )
import qualified Server.ResponseController     as ResponseController

import           Data.IORef                     ( IORef
                                                , modifyIORef'
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Data.Maybe                     ( isJust )
import           Language.LSP.Server            ( MonadLsp
                                                , getConfig
                                                )
import qualified Language.LSP.Types            as LSP
import           Options

--------------------------------------------------------------------------------

data Env = Env
  { envOptions            :: Options
  , envDevMode            :: Bool
  , envConfig             :: Config
  , envLogChan            :: Chan Text
  , envCommandController  :: CommandController
  , envResponseChan       :: Chan Response
  , envResponseController :: ResponseController
  }

createInitEnv :: (MonadIO m, MonadLsp Config m) => Options -> m Env
createInitEnv options =
  Env options (isJust (optViaTCP options))
    <$> getConfig
    <*> liftIO newChan
    <*> liftIO CommandController.new
    <*> liftIO newChan
    <*> liftIO ResponseController.new

--------------------------------------------------------------------------------

-- | OUR monad
type ServerM m = ReaderT Env m

runServerM :: Env -> ServerM m a -> m a
runServerM = flip runReaderT

--------------------------------------------------------------------------------

writeLog :: (Monad m, MonadIO m) => Text -> ServerM m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

writeLog' :: (Monad m, MonadIO m, Show a) => a -> ServerM m ()
writeLog' x = do
  chan <- asks envLogChan
  liftIO $ writeChan chan $ pack $ show x

-- | Provider
provideCommand :: (Monad m, MonadIO m) => IOTCM -> ServerM m ()
provideCommand iotcm = do
  controller <- asks envCommandController
  liftIO $ CommandController.put controller iotcm

-- | Consumter
consumeCommand :: (Monad m, MonadIO m) => Env -> m IOTCM
consumeCommand env = liftIO $ CommandController.take (envCommandController env)

waitUntilResponsesSent :: (Monad m, MonadIO m) => ServerM m ()
waitUntilResponsesSent = do
  controller <- asks envResponseController
  liftIO $ ResponseController.setCheckpointAndWait controller

signalCommandFinish :: (Monad m, MonadIO m) => ServerM m ()
signalCommandFinish = do
  writeLog "[Command] Finished"
  -- send `ResponseEnd`
  env <- ask
  liftIO $ writeChan (envResponseChan env) ResponseEnd
  -- allow the next Command to be consumed
  liftIO $ CommandController.release (envCommandController env)

-- | Sends a Response to the client via "envResponseChan"
sendResponse :: (Monad m, MonadIO m) => Env -> Response -> TCMT m ()
sendResponse env response = liftIO $ writeChan (envResponseChan env) response
