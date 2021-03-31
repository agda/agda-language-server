{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (run) where

-- entry point of the LSP server

import Common
import qualified Agda.Parser as Parser
import Control.Monad.Reader
import qualified Agda
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Text (pack)
import GHC.Generics (Generic)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Language.LSP.Server
import qualified Language.LSP.VFS as VFS
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import qualified Network.Simple.TCP as TCP
import Network.Socket (socketToHandle)
import qualified Switchboard
import Switchboard (Switchboard)
import Control.Concurrent

-- import Agda.Interaction.Highlighting.Generate
--     ( generateTokenInfoFromString )
-- import Agda.Interaction.Highlighting.Generate
--     ( generateTokenInfoFromString )
import Agda.Position (makeOffsetTable, toPositionWithoutFile, prettyPositionWithoutFile)
import Agda.Misc (onHover)
-- import Agda.Position (fromLSPPosition)

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
          _change = Just changeOptions, -- receive change notifications from the client
          _willSave = Just False, -- receive willSave notifications from the client
          _willSaveWaitUntil = Just False, -- receive willSave notifications from the client
          _save = Just $ InR saveOptions
        }

    changeOptions :: TextDocumentSyncKind
    changeOptions = TdSyncIncremental

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
          JSON.Error msg -> return $ CmdRes $ Just $ CmdErrCannotDecodeJSON $ show msg ++ "\n" ++ show params
          JSON.Success request -> handleCommandReq request
        -- respond with the Response
        responder $ Right $ JSON.toJSON response,

      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req

        result <- onHover uri pos
        responder $ Right result
        -- result <- getVirtualFile (toNormalizedUri uri)
        -- case result of 
        --   Nothing -> responder (Right Nothing)
        --   Just file -> do 
        --     let source = VFS.virtualFileText file



            -- let offsetTable = makeOffsetTable source
            -- let agdaPos = toPositionWithoutFile offsetTable pos
            -- lookupResult <- Parser.tokenAt uri source agdaPos
            -- case lookupResult of 
            --   Nothing -> responder (Right Nothing)
            --   Just (token, text) -> do 
            --     let range = Range pos pos
            --     let content = HoverContents $ markedUpContent "agda-language-server" text
            --     responder (Right $ Just $ Hover content (Just range))
    ]


--------------------------------------------------------------------------------

handleCommandReq :: CommandReq -> LspT () ServerM CommandRes
handleCommandReq request = do
  -- -- convert Request to LSP side effects
  -- toLSPSideEffects i request
  -- convert Request to Response
  toCommandRes request
  where
    toCommandRes :: CommandReq -> LspT () ServerM CommandRes
    toCommandRes CmdReqSYN = return $ CmdResACK Agda.getAgdaVersion
    toCommandRes (CmdReq cmd) = do
      case Agda.parseIOTCM cmd of
        Left err -> do
          lift $ writeLog $ "[Error] CmdErrCannotParseCommand:\n" <> pack err
          return $ CmdRes (Just (CmdErrCannotParseCommand err))
        Right iotcm -> do
          lift $ do
            writeLog $ "[Request] " <> pack (show cmd)
            provideCommand iotcm
            return $ CmdRes Nothing

--------------------------------------------------------------------------------

data CommandReq
  = CmdReqSYN -- ^ Client initiates a 2-way handshake
  | CmdReq String
  deriving (Generic)

instance FromJSON CommandReq

data CommandRes
  = CmdResACK -- ^ Server complets the 2-way handshake
      String   -- ^ Version number of Agda
  | CmdRes -- ^ The response for 'CmdReq'
      (Maybe CommandErr) -- ^ 'Nothing' to indicate success
  deriving (Generic)

instance ToJSON CommandRes

data CommandErr
  = CmdErrCannotDecodeJSON String
  | CmdErrCannotParseCommand String
  deriving (Generic)

instance ToJSON CommandErr
