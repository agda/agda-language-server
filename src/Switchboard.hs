{-# LANGUAGE OverloadedStrings #-}

module Switchboard (Switchboard, new, setupLanguageContextEnv, destroy) where

import Common
import Control.Concurrent
import qualified Control.Concurrent.Foreman as Foreman
import Control.Monad.Reader
import qualified Agda
import qualified Data.Aeson as JSON
import qualified Data.Text.IO as Text
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Data.IORef

data Switchboard = Switchboard 
  { sbPrintLog :: ThreadId
  , sbSendReaction :: ThreadId
  , sbRunAgda :: ThreadId
  , sbLanguageContextEnv :: IORef (Maybe (LanguageContextEnv ()))
  }

-- | All channels go in and out from here
new :: Env -> IO Switchboard
new env = do 
  ctxEnvIORef <- newIORef Nothing
  Switchboard
    <$> forkIO (keepPrintingLog env)
    <*> forkIO (keepSendindReaction env ctxEnvIORef)
    <*> forkIO (runReaderT Agda.interact env)
    <*> pure ctxEnvIORef

-- | For sending reactions to the client 
setupLanguageContextEnv :: Switchboard -> LanguageContextEnv () -> IO ()
setupLanguageContextEnv switchboard ctxEnv = do 
  writeIORef (sbLanguageContextEnv switchboard) (Just ctxEnv)

destroy :: Switchboard -> IO ()
destroy switchboard = do
  killThread (sbPrintLog switchboard)
  killThread (sbSendReaction switchboard)
  killThread (sbRunAgda switchboard)
  writeIORef (sbLanguageContextEnv switchboard) Nothing

-- | Keep printing log
-- Consumer of `envLogChan`
keepPrintingLog :: Env -> IO ()
keepPrintingLog env = forever $ do
  result <- readChan (envLogChan env)
  when (envDevMode env) $ do
    Text.putStrLn result

-- | Keep sending reactions
-- Consumer of `envReactionChan`
keepSendindReaction :: Env -> IORef (Maybe (LanguageContextEnv ())) -> IO ()
keepSendindReaction env ctxEnvIORef = forever $ do
  response <- readChan (envReactionChan env)

  result <- readIORef ctxEnvIORef
  forM_ result $ \ctxEnv -> do 
    runLspT ctxEnv $ do
      callback <- liftIO $ Foreman.dispatch (envReactionController env)

      let value = JSON.toJSON response
      sendRequest (SCustomMethod "agda") value $ \_result -> liftIO $ do
        -- writeChan (envLogChan env) $ "[Reaction] >>>> " <> pack (show value)
        callback ()