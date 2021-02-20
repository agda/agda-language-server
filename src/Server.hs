{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (run) where

-- entry point of the LSP server

import Common
import Control.Concurrent (ThreadId)
import qualified Control.Exception as Exception
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

--------------------------------------------------------------------------------

-- | Start a TCP server that accepts incoming connections and handles them
-- concurrently in different threads.
serveOnce ::
  MonadIO m =>
  -- | Host to bind.
  TCP.HostPreference ->
  -- | Server service port name or number to bind.
  TCP.ServiceName ->
  -- | Computation to run in a different thread once an incoming connection is
  -- accepted. Takes the connection socket and remote end address.
  ((TCP.Socket, TCP.SockAddr) -> IO ()) ->
  m (Either Exception.SomeException ThreadId)
serveOnce hp port k = liftIO $ do
  TCP.listen hp port $ \(lsock, _) -> do
    Exception.try (TCP.acceptFork lsock k)

run :: Bool -> IO Int
run devMode = do
  env <- createInitEnv devMode

  if devMode
    then do
      let port = "4000"
      putStrLn $ "[Server] Start accepting connections from port " <> port
      -- expecting to be probed by the client first
      _ <- serveOnce (TCP.Host "localhost") port $ \(_sock, _remoteAddr) -> return ()
      putStrLn "[Server] Probed" 
      -- here establishes the real connection
      connectionResult <- serveOnce (TCP.Host "localhost") port $ \(sock, _remoteAddr) -> do
        handle <- socketToHandle sock ReadWriteMode
        _ <- runServerWithHandles handle handle (serverDefn env)
        return ()

      case connectionResult of
        Left e -> do
          print e
          return 1
        Right _ -> return 0
    else do
      runServer (serverDefn env)
  where
    serverDefn :: Env -> ServerDefinition ()
    serverDefn env =
      ServerDefinition
        { onConfigurationChange = const $ pure $ Right (),
          doInitialize = \ctxEnv _req -> do
            Switchboard.run env ctxEnv
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
