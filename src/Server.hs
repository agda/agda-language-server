{-# LANGUAGE DeriveGeneric #-}

-- entry point of the LSP server

module Server
  ( run
  ) where


import qualified Agda
import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Text                      ( pack )
import           GHC.Generics                   ( Generic )
import           GHC.IO.IOMode                  ( IOMode(ReadWriteMode) )
import           Language.LSP.Server
import           Language.LSP.Types      hiding ( TextDocumentSyncClientCapabilities(..)
                                                )
import           Monad
import qualified Network.Simple.TCP            as TCP
import           Network.Socket                 ( socketToHandle )
import qualified Switchboard
import           Switchboard                    ( Switchboard )

import           Data.Maybe                     ( isJust )
import qualified Server.Handler                as Handler

import           Agda.Utils.Lens                ( (^.) )
import           Language.LSP.Types.Lens hiding ( options
                                                , textDocumentSync
                                                )


--------------------------------------------------------------------------------

run :: Maybe Int -> IO Int
run viaTCP = do
  env         <- createInitEnv (isJust viaTCP)
  switchboard <- Switchboard.new env
  case viaTCP of
    Just port -> do
      void
        $ TCP.serve (TCP.Host "127.0.0.1") (show port)
        $ \(sock, _remoteAddr) -> do
            writeChan (envLogChan env) "[Server] connection established"
            handle <- socketToHandle sock ReadWriteMode
            _ <- runServerWithHandles handle handle (serverDefn env switchboard)
            return ()
      Switchboard.destroy switchboard
      return 0
    Nothing -> do
      runServer (serverDefn env switchboard)
 where
  serverDefn :: Env -> Switchboard -> ServerDefinition ()
  serverDefn env switchboard = ServerDefinition
    { defaultConfig         = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize          = \ctxEnv _req -> do
                                Switchboard.setupLanguageContextEnv switchboard ctxEnv
                                pure $ Right ctxEnv
    , staticHandlers        = handlers
    , interpretHandler = \ctxEnv -> Iso (runLspT ctxEnv . runServerM env) liftIO
    , options               = lspOptions
    }
  lspOptions :: Options
  lspOptions = defaultOptions { textDocumentSync = Just syncOptions }

  -- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
  syncOptions :: TextDocumentSyncOptions
  syncOptions = TextDocumentSyncOptions { _openClose = Just True -- receive open and close notifications from the client
                                        , _change = Just changeOptions -- receive change notifications from the client
                                        , _willSave = Just False -- receive willSave notifications from the client
                                        , _willSaveWaitUntil = Just False -- receive willSave notifications from the client
                                        , _save = Just $ InR saveOptions
                                        }

  changeOptions :: TextDocumentSyncKind
  changeOptions = TdSyncIncremental

  -- includes the document content on save, so that we don't have to read it from the disk
  saveOptions :: SaveOptions
  saveOptions = SaveOptions (Just True)

-- handlers of the LSP server
handlers :: Handlers (ServerM (LspM ()))
handlers = mconcat
  [   -- custom methods, not part of LSP
    requestHandler (SCustomMethod "agda") $ \req responder -> do
    let RequestMessage _ _i _ params = req
    -- JSON Value => Request => Response
    response <- case JSON.fromJSON params of
      JSON.Error msg ->
        return
          $  CmdRes
          $  Just
          $  CmdErrCannotDecodeJSON
          $  show msg
          ++ "\n"
          ++ show params
      JSON.Success request -> handleCommandReq request
    -- respond with the Response
    responder $ Right $ JSON.toJSON response
  ,
        -- hover provider
    requestHandler STextDocumentHover $ \req responder -> do
    let
      RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone)
        = req
    result <- Handler.onHover uri pos
    responder $ Right result
  -- -- syntax highlighting 
  -- , requestHandler STextDocumentSemanticTokensFull $ \req responder -> do
  --   result <- Handler.onHighlight (req ^. (params . textDocument . uri))
  --   responder result
  ]

--------------------------------------------------------------------------------

-- | Convert "CommandReq" to "CommandRes"
handleCommandReq :: CommandReq -> ServerM (LspM ()) CommandRes
handleCommandReq CmdReqSYN    = return $ CmdResACK Agda.getAgdaVersion
handleCommandReq (CmdReq cmd) = do
  case Agda.parseIOTCM cmd of
    Left err -> do
      writeLog $ "[Error] CmdErrCannotParseCommand:\n" <> pack err
      return $ CmdRes (Just (CmdErrCannotParseCommand err))
    Right iotcm -> do
      writeLog $ "[Request] " <> pack (show cmd)
      provideCommand iotcm
      return $ CmdRes Nothing

--------------------------------------------------------------------------------

data CommandReq
  = CmdReqSYN -- ^ For client to initiate a 2-way handshake
  | CmdReq String
  deriving (Generic)

instance FromJSON CommandReq

data CommandRes
  = CmdResACK -- ^ For server to complete a 2-way handshake
      String   -- ^ Version number of Agda
  | CmdRes -- ^ Response for 'CmdReq'
      (Maybe CommandErr) -- ^ 'Nothing' to indicate success
  deriving (Generic)

instance ToJSON CommandRes

data CommandErr
  = CmdErrCannotDecodeJSON String
  | CmdErrCannotParseCommand String
  deriving (Generic)

instance ToJSON CommandErr
