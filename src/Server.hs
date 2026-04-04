{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- entry point of the LSP server

module Server (run) where

import qualified Agda
import Control.Monad (void)
import Control.Monad.Reader (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import Data.Text (pack)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types (HoverParams (..), SaveOptions (..), TextDocumentIdentifier (..), TextDocumentSyncKind (..), TextDocumentSyncOptions (..), type (|?) (..))
import Language.LSP.Server hiding (Options)
import qualified Language.LSP.Server as LSP
import Monad
import qualified Network.Simple.TCP as TCP
import Network.Socket (socketToHandle)
import Options
import qualified Server.Handler as Handler
import Switchboard (agdaCustomMethod)
import qualified Switchboard
import Server.Handler.TextDocument.DocumentSymbol (documentSymbolHandler)
import Server.Handler.TextDocument.FileManagement (didOpenHandler, didCloseHandler, didSaveHandler)
import System.IO (stdout, stdin)
import Colog.Core (LogAction, WithSeverity)
import qualified Colog.Core as L
import Prettyprinter (viaShow, pretty)
import Language.LSP.Logging (defaultClientLogger)
import qualified Data.Text as T

#if defined(wasm32_HOST_ARCH)
import Agda.Utils.IO (catchIO)
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (stdInput, setFdOption, FdOption (..))
#endif

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
#if defined(wasm32_HOST_ARCH)
      liftIO $ setFdOption stdInput NonBlockingRead True
        `catchIO` (\ (e :: IOError) -> hPutStrLn stderr $ "Failed to enable nonblocking on stdin: " ++ (show e) ++ "\nThe WASM module might not behave correctly.")
#endif
      runServerWithHandles defaultIOLogger defaultLspLogger stdin stdout (serverDefn options)
  where
    serverDefn :: Options -> ServerDefinition Config
    serverDefn options =
      ServerDefinition
        { defaultConfig = initConfig,
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
            Iso
              { forward = runLspT ctxEnv . runServerT env,
                backward = liftIO
              },
          options = lspOptions
        }

-- Unexported by lsp library
defaultIOLogger :: LogAction IO (WithSeverity LspServerLog)
defaultIOLogger = L.cmap (show . prettyMsg) L.logStringStderr
 where
  prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)

-- Modified from lsp library to remove stderr
defaultLspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
defaultLspLogger = L.cmap (fmap (T.pack . show . pretty)) defaultClientLogger

lspOptions :: LSP.Options
lspOptions = LSP.defaultOptions {optTextDocumentSync = Just syncOptions}

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True, -- receive open and close notifications from the client
      _change = Just TextDocumentSyncKind_Incremental, -- receive change notifications from the client
      _willSave = Just False, -- receive willSave notifications from the client
      _willSaveWaitUntil = Just False, -- receive willSave notifications from the client
      _save = Just $ InR $ SaveOptions (Just False)
    }

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers =
  mconcat
    [ -- custom methods, not part of LSP
      requestHandler agdaCustomMethod $ \req responder -> do
        let TRequestMessage _ _i _ params = req
        response <- Agda.sendCommand params
        responder $ Right response,
      -- `textDocument/hover`
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
        result <- Handler.onHover uri pos
        responder $ Right result,
      documentSymbolHandler,
      -- -- syntax highlighting
      -- , requestHandler STextDocumentSemanticTokensFull $ \req responder -> do
      --   result <- Handler.onHighlight (req ^. (params . textDocument . uri))
      --   responder result

      -- `initialized`
      notificationHandler SMethod_Initialized $ \_notification -> return (),
      -- `workspace/didChangeConfiguration`
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_notification -> return (),
      -- `textDocument/didOpen`
      didOpenHandler,
      -- `textDocument/didClose`
      didCloseHandler,
      -- `textDocument/didChange`
      notificationHandler SMethod_TextDocumentDidChange $ \_notification -> return (),
      -- `textDocument/didSave`
      didSaveHandler
    ]
