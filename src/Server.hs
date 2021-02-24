{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (run) where

-- entry point of the LSP server

import Common
import Control.Monad.Reader
import qualified Agda
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Text (pack)
import GHC.Generics (Generic)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import qualified Network.Simple.TCP as TCP
import Network.Socket (socketToHandle)
import qualified Switchboard
import Switchboard (Switchboard)
import Control.Concurrent

--------------------------------------------------------------------------------

run :: Bool -> IO Int
run devMode = do
  env <- createInitEnv devMode
  switchboard <- Switchboard.new env

  if devMode
    then do
      let port = "4096"

      void $ TCP.serve (TCP.Host "127.0.0.1") port $ \(sock, _remoteAddr) -> do
        writeChan (envLogChan env) "[Server] connection established"
        handle <- socketToHandle sock ReadWriteMode
        _ <- runServerWithHandles handle handle (serverDefn env switchboard)
        return ()

      Switchboard.destroy switchboard
      return 0
    else do
      runServer (serverDefn env switchboard)
  where
    serverDefn :: Env -> Switchboard -> ServerDefinition ()
    serverDefn env switchboard =
      ServerDefinition
        { onConfigurationChange = const $ pure $ Right (),
          doInitialize = \ctxEnv _req -> do
            Switchboard.setupLanguageContextEnv switchboard ctxEnv
            pure $ Right ctxEnv,
          staticHandlers = handlers,
          interpretHandler = \ctxEnv -> Iso (runServerLSP env ctxEnv) liftIO,
          options = lspOptions
        }
    lspOptions :: Options
    lspOptions =
      defaultOptions
        { textDocumentSync = Just syncOptions
        }

    -- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
    syncOptions :: TextDocumentSyncOptions
    syncOptions =
      TextDocumentSyncOptions
        { _openClose = Just True, -- receive open and close notifications from the client
          _change = Nothing, -- receive change notifications from the client
          _willSave = Just False, -- receive willSave notifications from the client
          _willSaveWaitUntil = Just False, -- receive willSave notifications from the client
          _save = Just $ InR saveOptions
        }

    -- includes the document content on save, so that we don't have to read it from the disk
    saveOptions :: SaveOptions
    saveOptions = SaveOptions (Just True)

-- handlers of the LSP server
handlers :: Handlers (LspT () ServerM)
handlers =
  mconcat
    [ -- custom methods, not part of LSP
      requestHandler (SCustomMethod "agda") $ \req responder -> do
        let RequestMessage _ _i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> return $ ResCannotDecodeRequest $ show msg ++ "\n" ++ show params
          JSON.Success request -> handleRequest request
        -- respond with the Response
        responder $ Right $ JSON.toJSON response
    ]

--------------------------------------------------------------------------------

handleRequest :: Request -> LspT () ServerM Response
handleRequest request = do
  -- -- convert Request to LSP side effects
  -- toLSPSideEffects i request
  -- convert Request to Response
  toResponse request
  where
    toResponse :: Request -> LspT () ServerM Response
    toResponse ReqInitialize = return $ ResInitialize Agda.getAgdaVersion
    toResponse (ReqCommand cmd) = do
      case Agda.parseIOTCM cmd of
        Left err -> do
          lift $ writeLog $ "Error: parseIOTCM" <> pack err
          return ResCommand
        Right iotcm -> do
          lift $ do
            writeLog $ "[Request] " <> pack (show cmd)
            provideCommand iotcm
            return ResCommand

--------------------------------------------------------------------------------

data Request 
  = ReqInitialize -- ^ This Request comes before anything else
  | ReqCommand String
  deriving (Generic)

instance FromJSON Request

--------------------------------------------------------------------------------

-- | Response
data Response
  = ResInitialize -- ^ The response for 'ReqInitialize'
      String      -- ^ Version number of Agda
  | ResCommand
  | ResCannotDecodeRequest String
  deriving (Generic)

instance ToJSON Response
