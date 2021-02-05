{-# LANGUAGE OverloadedStrings #-}

module Common where 

import Control.Concurrent
import Data.Text (Text)
import Control.Monad.Reader
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)
import Agda.Interaction.Base (IOTCM)
import Agda.Interaction.Response (Response (..))
import Agda.TypeChecking.Monad (TCMT)

import Control.Throttler (Throttler)
import qualified Control.Throttler as Throttler
import Data.IORef

--------------------------------------------------------------------------------


data ResponsePacket = ResponsePacket Response String | ResponseDone (MVar ())

data Env = Env
  { envLogChan :: Chan Text
  , envCmdThrottler :: Throttler IOTCM 
  , envCmdDoneCallback :: IORef (Maybe (() -> IO ())) 
  , envResponseChan :: Chan ResponsePacket
  , envDevMode :: Bool
  }

type ServerM' m = ReaderT Env m
type ServerM = ServerM' IO

createInitEnv :: Bool -> IO Env 
createInitEnv devMode = 
  Env <$> newChan 
    <*> Throttler.new
    <*> newIORef Nothing
    <*> newChan
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
  writeLog "[Command] command issued"
  liftIO $ Throttler.put throttler iotcm 
  writeLog "[Command] command handled!"

-- | Consumter
consumeCommand :: (Monad m, MonadIO m) => Env -> m IOTCM 
consumeCommand env = liftIO $ do 
  (iotcm, callback) <- Throttler.take (envCmdThrottler env)
  -- store the callback, so that we can signal when we are done
  writeIORef (envCmdDoneCallback env) (Just callback)
  return iotcm

waitUntilResponsesSent :: (Monad m, MonadIO m) => ServerM' m ()  
waitUntilResponsesSent = do 
  responseChan <- asks envResponseChan 
  lock <- liftIO newEmptyMVar 
  liftIO $ writeChan responseChan (ResponseDone lock)
  liftIO $ takeMVar lock

signalCommandFinish :: (Monad m, MonadIO m) => ServerM' m () 
signalCommandFinish = do 
  callbackRef <- asks envCmdDoneCallback
  result <- liftIO $ readIORef callbackRef
  case result of 
    Nothing -> return ()
    Just callback -> do 
      liftIO $ callback ()


sendResponse :: (Monad m, MonadIO m) => Env -> (Response, String) -> TCMT m ()
sendResponse env (response, lispified) = do
  liftIO $ writeChan (envResponseChan env) (ResponsePacket response lispified)

recvResponse :: (Monad m, MonadIO m) => ServerM' m ResponsePacket
recvResponse = do
  chan <- asks envResponseChan
  writeLog "[Response] waiting ..."
  response <- liftIO $ readChan chan
  writeLog "[Response] reveived"
  return response