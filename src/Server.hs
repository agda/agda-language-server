{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (run) where

-- entry point of the LSP server

-- import Language.LSP.Types (TextDocumentSyncOptions(..), SaveOptions(..))

import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import Data.Text (Text)

import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

run :: IO Int
run = runServer serverDefn
  where
    serverDefn :: ServerDefinition ()
    serverDefn =
      ServerDefinition
        { onConfigurationChange = const $ pure $ Right (),
          doInitialize = \ctxEnv _req -> pure $ Right ctxEnv,
          staticHandlers = handlers,
          interpretHandler = \ctxEnv -> Iso (runLspT ctxEnv) liftIO,
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
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ -- custom methods, not part of LSP
      requestHandler (SCustomMethod "agda") $ \req responder -> do
        let RequestMessage _ i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> return $ ResCannotDecodeRequest $ show msg ++ "\n" ++ show params
          JSON.Success request -> handleRequest request
        -- respond with the Response
        responder $ Right $ JSON.toJSON response
    ]

--------------------------------------------------------------------------------

handleRequest :: Request -> LspT () IO Response
handleRequest request = do
  -- -- convert Request to LSP side effects
  -- toLSPSideEffects i request
  -- convert Request to Response
  toResponse request
  where 
    toResponse :: Request -> LspT () IO Response
    toResponse ReqInitialize = return $ ResInitialize "2.6.1.1"

--------------------------------------------------------------------------------
-- | Request
-- data ReqKind
--   = ReqInitialize
--   deriving (Generic)

-- instance FromJSON ReqKind

-- data Request = Req FilePath ReqKind
data Request = ReqInitialize
  deriving (Generic)

instance FromJSON Request


--------------------------------------------------------------------------------

-- | Response
-- data ResKind
--   = ResInitialize
--   deriving (Generic)

data Response = ResInitialize String | ResCannotDecodeRequest String
  deriving (Generic)

instance ToJSON Response
