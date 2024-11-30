{-# LANGUAGE CPP #-}

-- entry point of the LSP server

module Server
  ( run,
  )
where

import qualified Agda
import           Control.Concurrent             ( writeChan )
import           Control.Monad                  ( void )
import           Control.Monad.Reader           ( MonadIO(liftIO) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Text                      ( pack )
import           GHC.IO.IOMode                  ( IOMode(ReadWriteMode) )
import           Language.LSP.Server     hiding ( Options )
import           Language.LSP.Protocol.Message  ( pattern RequestMessage
                                                , SMethod( SMethod_CustomMethod, SMethod_TextDocumentHover)
                                                , pattern TRequestMessage
                                                )
import           Language.LSP.Protocol.Types    ( TextDocumentSyncOptions(..)
                                                , TextDocumentSyncKind( TextDocumentSyncKind_Incremental )
                                                , ServerCapabilities (_textDocumentSync )
                                                , SaveOptions( SaveOptions )
                                                , pattern TextDocumentIdentifier
                                                , pattern HoverParams
                                                , pattern InR
                                                , pattern InL
                                                )
import           Monad
import qualified Network.Simple.TCP            as TCP
import           Network.Socket                 ( socketToHandle )
import qualified Switchboard
import           Switchboard                    ( Switchboard, agdaCustomMethod )

import qualified Server.Handler                as Handler

import qualified Language.LSP.Server           as LSP
import           Options

--------------------------------------------------------------------------------

run :: Options -> IO Int
run options = do
  case optViaTCP options of
    Just port -> do
      void $
        TCP.serve (TCP.Host "127.0.0.1") (show port) $
          \(sock, _remoteAddr) -> do
            -- writeChan (envLogChan env) "[Server] connection established"
            handle <- socketToHandle sock ReadWriteMode
            _ <- runServerWithHandles mempty mempty handle handle (serverDefn options)
            return ()
      -- Switchboard.destroy switchboard
      return 0
    Nothing -> do
      runServer (serverDefn options)
  where
    serverDefn :: Options -> ServerDefinition Config
    serverDefn options =
      ServerDefinition
        { 
          defaultConfig = initConfig,
          onConfigChange = const $ pure (),
          parseConfig = \old newRaw -> case JSON.fromJSON newRaw of
            JSON.Error s -> Left $ pack $ "Cannot parse server configuration: " <> s
            JSON.Success new -> Right new,
          doInitialize = \ctxEnv _req -> do
            env <- runLspT ctxEnv (createInitEnv options)
            switchboard <- Switchboard.new env
            Switchboard.setupLanguageContextEnv switchboard ctxEnv
            pure $ Right (ctxEnv, env),
          configSection = "dummy",
          staticHandlers = const handlers,
          interpretHandler = \(ctxEnv, env) ->
            Iso {
              forward = runLspT ctxEnv . runServerM env,
              backward = liftIO
            },
          options = lspOptions
        }

    lspOptions :: LSP.Options
    lspOptions = defaultOptions { optTextDocumentSync = Just syncOptions }

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
    changeOptions = TextDocumentSyncKind_Incremental

    -- includes the document content on save, so that we don't have to read it from the disk
    saveOptions :: SaveOptions
    saveOptions = SaveOptions (Just True)

-- handlers of the LSP server
handlers :: Handlers (ServerM (LspM Config))
handlers = mconcat
  [ -- custom methods, not part of LSP
    requestHandler agdaCustomMethod $ \ req responder -> do
      let TRequestMessage _ _i _ params = req
      response <- Agda.sendCommand params
      responder $ Right response
  ,
        -- hover provider
    requestHandler hoverMethod $ \ req responder -> do
      let TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
      result <- Handler.onHover uri pos
      responder $ Right result
  -- -- syntax highlighting
  -- , requestHandler STextDocumentSemanticTokensFull $ \req responder -> do
  --   result <- Handler.onHighlight (req ^. (params . textDocument . uri))
  --   responder result
  ]
  where
    hoverMethod = SMethod_TextDocumentHover
