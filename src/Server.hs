{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (run) where

-- entry point of the LSP server

-- import Language.LSP.Types (TextDocumentSyncOptions(..), SaveOptions(..))

import qualified Agda.Interaction.Base as Agda
import qualified Agda.Interaction.Response as Agda
import Common
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Core
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Lispify (responseAbbr)
import Network.Simple.TCP (HostPreference (Host), serve)
import Network.Socket (socketToHandle)
import qualified Control.Concurrent.Foreman as Foreman

--------------------------------------------------------------------------------

run :: Bool -> IO Int
run devMode = do
  env <- createInitEnv devMode

  if devMode
    then do
      let port = "4000"
      --

      putStrLn $ "== opening a dev server at port " ++ port ++ " =="
      serve (Host "localhost") port $ \(sock, _remoteAddr) -> do
        putStrLn "== connection established =="
        handle <- socketToHandle sock ReadWriteMode
        _ <- runServerWithHandles handle handle (serverDefn env)
        putStrLn "== connection closed =="
    else do
      runServer (serverDefn env)
  where
    -- keeps reading and printing from `envLogChan`
    keepPrintLog :: Env -> IO ()
    keepPrintLog env = do
      result <- readChan (envLogChan env)
      when (envDevMode env) $ do
        Text.putStrLn result
      keepPrintLog env

    keepSendindResponse :: Env -> LanguageContextEnv () -> IO ()
    keepSendindResponse env ctxEnv = do
      (_response, lispified) <- readChan (envResponseChan env)
      runLspT ctxEnv $ do
        callback <- liftIO $ Foreman.dispatch (envResponseController env)
        
        let value = JSON.toJSON lispified
        sendRequest (SCustomMethod "agda") value $ \result -> liftIO $ do 
          writeChan (envLogChan env) $ "[Response] " <> responseAbbr _response <> " received"
          callback ()
          
        liftIO $ writeChan (envLogChan env) $ "[Response] " <> responseAbbr _response <> " sent"
        -- liftIO $ callback ()

      -- case responsePacket of
      --   ResponsePacket _response lispified -> do
      --     runLspT ctxEnv $ do
      --       callback <- liftIO $ Foreman.dispatch (envResponseController env)
            
      --       let value = JSON.toJSON lispified
      --       sendRequest (SCustomMethod "agda") value $ \result -> liftIO $ do 
      --         writeChan (envLogChan env) $ "[Response] " <> responseAbbr _response <> " received"
      --         callback ()
              
      --       liftIO $ writeChan (envLogChan env) $ "[Response] " <> responseAbbr _response <> " sent"
      --   ResponseDone mvar -> do
      --     liftIO $ writeChan (envLogChan env) "[Response] Done"
      --     putMVar mvar ()

      keepSendindResponse env ctxEnv

    serverDefn :: Env -> ServerDefinition ()
    serverDefn env =
      ServerDefinition
        { onConfigurationChange = const $ pure $ Right (),
          doInitialize = \ctxEnv _req -> do
            putStrLn "[LSP] doInitialize"
            forkIO $ keepPrintLog env
            forkIO $ runReaderT Core.interact env
            forkIO $ keepSendindResponse env ctxEnv
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
    [ -- notificationHandler SInitialized $ \_ -> do
      --   env <- lift ask
      --   _ <- liftIO $ forkIO $ do

      --     envResponseChan env
      --     return ()

      --     -- lift $ writeLog "delay start"
      --     -- liftIO $ threadDelay 10000000
      --     -- lift $ writeLog "delay end"
      --   return (),
      -- custom methods, not part of LSP
      requestHandler (SCustomMethod "agda") $ \req responder -> do
        let RequestMessage _ i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> return $ ResCannotDecodeRequest $ show msg ++ "\n" ++ show params
          JSON.Success request -> handleRequest request
        -- respond with the Response
        responder $ Right $ JSON.toJSON response
    ]

-- keepReadingResponse :: Env -> IO ()
--------------------------------------------------------------------------------

handleRequest :: Request -> LspT () ServerM Response
handleRequest request = do
  -- -- convert Request to LSP side effects
  -- toLSPSideEffects i request
  -- convert Request to Response
  toResponse request
  where
    toResponse :: Request -> LspT () ServerM Response
    toResponse ReqInitialize = return $ ResInitialize Core.getAgdaVersion
    toResponse (ReqCommand cmd) = do
      case Core.parseIOTCM cmd of
        Left error -> do
          lift $ writeLog $ "Error: parseIOTCM" <> pack error
          return ResCommandDone
        Right iotcm -> do
          lift $ do
            writeLog $ "[Request] " <> pack (show cmd)
            -- issue the command, block until it is handled
            provideCommand iotcm
            return ResCommandDone

--------------------------------------------------------------------------------

-- | Request
-- data ReqKind
--   = ReqInitialize
--   deriving (Generic)

-- instance FromJSON ReqKind

-- data Request = Req FilePath ReqKind
data Request = ReqInitialize | ReqCommand String
  deriving (Generic)

instance FromJSON Request

--------------------------------------------------------------------------------

-- | Response
-- data ResKind
--   = ResInitialize
--   deriving (Generic)
data Response
  = ResInitialize String
  | -- | ResResponse String
    ResCommandDone
  | ResCannotDecodeRequest String
  deriving (Generic)

instance ToJSON Response

--------------------------------------------------------------------------------

data Ntf = NtfResponse String | NtfDummy
  deriving (Generic)

instance ToJSON Ntf
