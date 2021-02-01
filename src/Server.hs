module Server ( run ) where


-- entry point of the LSP server
import Language.LSP.Server
-- import Language.LSP.Types (TextDocumentSyncOptions(..), SaveOptions(..))
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Control.Monad.IO.Class (liftIO)

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
      
    ]
