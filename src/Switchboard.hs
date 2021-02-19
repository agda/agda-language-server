{-# LANGUAGE OverloadedStrings #-}

module Switchboard (run) where

import Common
import Control.Concurrent
import qualified Control.Concurrent.Foreman as Foreman
import Control.Monad (when)
import Control.Monad.Reader
import qualified Agda
import qualified Data.Aeson as JSON
import qualified Data.Text.IO as Text
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))

-- | All channels go in and out from here
run :: Env -> LanguageContextEnv () -> IO ()
run env ctxEnv = do
  forkIO (keepPrintingLog env)
  forkIO (keepSendindReaction env ctxEnv)
  forkIO (runReaderT Agda.interact env)
  return ()

-- | Keep printing log
-- Consumer of `envLogChan`
keepPrintingLog :: Env -> IO ()
keepPrintingLog env = forever $ do
  result <- readChan (envLogChan env)
  when (envDevMode env) $ do
    Text.putStrLn result

-- | Keep sending reactions
-- Consumer of `envReactionChan`
keepSendindReaction :: Env -> LanguageContextEnv () -> IO ()
keepSendindReaction env ctxEnv = forever $ do
  response <- readChan (envReactionChan env)
  runLspT ctxEnv $ do
    callback <- liftIO $ Foreman.dispatch (envReactionController env)

    let value = JSON.toJSON response
    sendRequest (SCustomMethod "agda") value $ \result -> liftIO $ do
      -- writeChan (envLogChan env) $ "[Reaction] >>>> " <> pack (show value)
      callback ()