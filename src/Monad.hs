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

--------------------------------------------------------------------------------

data Env = Env
  { envLogChan            :: Chan Text
  , envCommandController  :: CommandController
  , envResponseChan       :: Chan Response
  , envResponseController :: ResponseController
  , envDevMode            :: Bool
  }

createInitEnv :: Bool -> IO Env
createInitEnv devMode =
  Env
    <$> newChan
    <*> CommandController.new
    <*> newChan
    <*> ResponseController.new
    <*> pure devMode

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
sendResponse env response = do
  case response of
    -- NOTE: highlighting-releated reponses are intercepted and converted to 
    ResponseHighlightingInfoDirect{} -> return ()
    ResponseHighlightingInfoIndirect{} -> return ()
    ResponseClearHighlightingTokenBased {} -> return ()
    ResponseClearHighlightingNotOnlyTokenBased {} -> return ()
    -- other kinds of responses
    _ -> liftIO $ writeChan (envResponseChan env) response
