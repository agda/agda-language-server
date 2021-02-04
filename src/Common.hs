{-# LANGUAGE OverloadedStrings #-}

module Common where 

import Control.Concurrent
import Data.Text (Text)
import Control.Monad.Reader
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)
import Agda.Interaction.Base (IOTCM)
import Agda.Interaction.Response (Response)
    
--------------------------------------------------------------------------------

data Env = Env
  { envLogChan :: Chan Text
  , envCommandChan :: Chan IOTCM 
  , envResponseChan :: Chan Response 
  , envCommandDoneVar :: MVar ()
  , envDevMode :: Bool
  }

type ServerM' m = ReaderT Env m
type ServerM = ServerM' IO

createInitEnv :: Bool -> IO Env 
createInitEnv devMode = 
  Env <$> newChan 
    <*> newChan
    <*> newChan
    <*> newEmptyMVar
    <*> pure devMode 


runServerLSP :: Env -> LanguageContextEnv () -> LspT () (ServerM' m) a -> m a
runServerLSP env ctxEnv program = runReaderT (runLspT ctxEnv program) env

writeLog :: (Monad m, MonadIO m) => Text -> ServerM' m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

signalCommandDone :: (Monad m, MonadIO m) => ServerM' m ()
signalCommandDone = do
  var <- asks envCommandDoneVar
  liftIO $ putMVar var ()
  writeLog "[CommandDone] signaled"

waitCommandDone :: (Monad m, MonadIO m) => ServerM' m ()
waitCommandDone = do
  writeLog "[CommandDone] waiting ..."
  var <- asks envCommandDoneVar
  liftIO $ takeMVar var
  writeLog "[CommandDone] received"

signalResponse :: (Monad m, MonadIO m) => Env -> Response -> m ()
signalResponse env response = do
  liftIO $ writeChan (envResponseChan env) response
  liftIO $ writeChan (envLogChan env) "[Response] signaled"

waitResponse :: (Monad m, MonadIO m) => ServerM' m Response
waitResponse = do
  chan <- asks envResponseChan
  writeLog "[Response] waiting ..."
  response <- liftIO $ readChan chan
  writeLog "[Response] reveived"
  return response