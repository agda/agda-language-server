{-# LANGUAGE OverloadedStrings #-}

module Common where 

import Control.Concurrent
import Data.Text (Text)
import Control.Monad.Reader
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)
import Agda.Interaction.Base (IOTCM)
import Agda.Interaction.Response (Response (..))
    
--------------------------------------------------------------------------------

data Env = Env
  { envLogChan :: Chan Text
  , envCommandVar :: MVar IOTCM 
  , envResponseChan :: Chan Response 
  , envCommandDoneVar :: MVar ()
  , envDevMode :: Bool
  }

type ServerM' m = ReaderT Env m
type ServerM = ServerM' IO

createInitEnv :: Bool -> IO Env 
createInitEnv devMode = 
  Env <$> newChan 
    <*> newEmptyMVar
    <*> newChan
    <*> newEmptyMVar
    <*> pure devMode 


runServerLSP :: Env -> LanguageContextEnv () -> LspT () (ServerM' m) a -> m a
runServerLSP env ctxEnv program = runReaderT (runLspT ctxEnv program) env

writeLog :: (Monad m, MonadIO m) => Text -> ServerM' m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

issueCommand :: (Monad m, MonadIO m) => IOTCM -> ServerM' m ()
issueCommand iotcm = do 
  cmdVar <- asks envCommandVar 
  writeLog "[Command] 1. trying to issue a command ..."
  liftIO $ putMVar cmdVar iotcm 
  writeLog "[Command] 2. command issued, waiting it to be handled ..."
  doneVar <- asks envCommandDoneVar
  liftIO $ takeMVar doneVar
  writeLog "[Command] 4. command handled!"


-- peek the command but don't take it away
-- only take it away after the command has been handled and all responses have been sent
peekCommand :: (Monad m, MonadIO m) => Env -> m IOTCM 
peekCommand env = do 
  liftIO $ readMVar (envCommandVar env) 

-- remove the command from the MVar to signal that it's been completed
-- so that the next command could come in 
completeCommand :: (Monad m, MonadIO m) => ServerM' m () 
completeCommand = do 
  -- unblock `envCommandVar`
  cmdVar <- asks envCommandVar 
  void $ liftIO $ takeMVar cmdVar
  -- signal complete 
  doneVar <- asks envCommandDoneVar 
  liftIO $ putMVar doneVar ()
  writeLog "[Command] 3. signal command handled"
  


sendResponse :: (Monad m, MonadIO m) => Env -> Response -> m ()
sendResponse env response = do
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
  liftIO $ writeChan (envResponseChan env) response
  liftIO $ writeChan (envLogChan env) $ "[Response] " <> message

recvResponse :: (Monad m, MonadIO m) => ServerM' m Response
recvResponse = do
  chan <- asks envResponseChan
  writeLog "[Response] waiting ..."
  response <- liftIO $ readChan chan
  writeLog "[Response] reveived"
  return response