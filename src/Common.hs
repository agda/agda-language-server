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

data Env = Env
  { envLogChan :: Chan Text
  , envCmdThrottler :: Throttler IOTCM 
  , envCmdDoneCallback :: IORef (Maybe (() -> IO ())) 
  , envResponseChan :: Chan (Response, String) 
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
  let message = case response of
        Resp_HighlightingInfo {} -> "Resp_HighlightingInfo"
        Resp_Status {} -> "Resp_Status"
        Resp_JumpToError {} -> "Resp_JumpToError"
        Resp_InteractionPoints {} -> "Resp_InteractionPoints"
        Resp_GiveAction {} -> "Resp_GiveAction"
        Resp_MakeCase {} -> "Resp_MakeCase"
        Resp_SolveAll {} -> "Resp_SolveAll"
        Resp_DisplayInfo {} -> "Resp_DisplayInfo"
        Resp_RunningInfo {} -> "Resp_RunningInfo"
        Resp_ClearRunningInfo {} -> "Resp_ClearRunningInfo"
        Resp_ClearHighlighting {} -> "Resp_ClearHighlighting"
        Resp_DoneAborting {} -> "Resp_DoneAborting"
        Resp_DoneExiting {} -> "Resp_DoneExiting"
  liftIO $ writeChan (envResponseChan env) (response, lispified)
  liftIO $ writeChan (envLogChan env) $ "[Response] " <> message

recvResponse :: (Monad m, MonadIO m) => ServerM' m (Response, String)
recvResponse = do
  chan <- asks envResponseChan
  writeLog "[Response] waiting ..."
  response <- liftIO $ readChan chan
  writeLog "[Response] reveived"
  return response