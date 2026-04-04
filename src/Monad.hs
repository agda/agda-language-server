{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Monad where

import Agda.IR
import Agda.Interaction.Base (IOTCM)
import Agda.TypeChecking.Monad (TCMT)
import qualified Agda.TypeChecking.Monad as TCM
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad.Reader
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.Maybe (isJust)
import Data.Text
  ( Text,
    pack,
  )
import Language.LSP.Server
  ( LspM,
    MonadLsp,
    getConfig,
  )
import qualified Language.LSP.Server as LSP
import Options
import Server.CommandController (CommandController)
import qualified Server.CommandController as CommandController
import Server.Filesystem (MonadFilesystem)
import qualified Server.Filesystem as FS
import Server.Model (Model)
import qualified Server.Model as Model
import Server.ResponseController (ResponseController)
import qualified Server.ResponseController as ResponseController
import Server.VfsIndex (VfsIndex)
import qualified Server.VfsIndex as VfsIndex

--------------------------------------------------------------------------------

data Env = Env
  { envOptions :: Options,
    envDevMode :: Bool,
    envConfig :: Config,
    envMockLsp :: Bool,
    envLogChan :: Chan Text,
    envCommandController :: CommandController,
    envResponseChan :: Chan Response,
    envResponseController :: ResponseController,
    envFilesystemProvider :: !FS.Layered,
    envVfsIndex :: !(IORef VfsIndex),
    envModel :: !(IORef Model)
  }

createInitEnv :: (MonadIO m, MonadLsp Config m) => Options -> m Env
createInitEnv options =
  Env options (isJust (optViaTCP options))
    <$> getConfig
    <*> (pure False)
    <*> liftIO newChan
    <*> liftIO CommandController.new
    <*> liftIO newChan
    <*> liftIO ResponseController.new
    <*> (pure $ FS.Layered [FS.Wrap FS.LspVirtualFilesystem, FS.Wrap FS.OsFilesystem])
    <*> liftIO (newIORef VfsIndex.empty)
    <*> liftIO (newIORef Model.mkEmpty)

--------------------------------------------------------------------------------

-- | OUR monad
type ServerT m = ReaderT Env m

type ServerM = ServerT (LspM Config)

runServerT :: Env -> ServerT m a -> m a
runServerT = flip runReaderT

instance MonadFilesystem ServerM where
  askVfsIndex = askVfsIndex

--------------------------------------------------------------------------------

writeLog :: (Monad m, MonadIO m) => Text -> ServerT m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

writeLog' :: (Monad m, MonadIO m, Show a) => a -> ServerT m ()
writeLog' x = do
  chan <- asks envLogChan
  liftIO $ writeChan chan $ pack $ show x

askFilesystemProvider :: (MonadIO m) => ServerT m FS.Layered
askFilesystemProvider = asks envFilesystemProvider

askModel :: (MonadIO m) => ServerT m Model
askModel = do
  modelVar <- asks envModel
  liftIO $ readIORef modelVar

modifyModel :: (MonadIO m) => (Model -> Model) -> ServerT m ()
modifyModel f = do
  modelVar <- asks envModel
  liftIO $ modifyIORef' modelVar f

askVfsIndex :: (MonadIO m) => ServerT m VfsIndex
askVfsIndex = do
  vfsIndexVar <- asks envVfsIndex
  liftIO $ readIORef vfsIndexVar

modifyVfsIndex :: (MonadIO m) => (VfsIndex -> VfsIndex) -> ServerT m ()
modifyVfsIndex f = do
  vfsIndexVar <- asks envVfsIndex
  liftIO $ modifyIORef' vfsIndexVar f

catchTCError :: ServerM a -> (TCM.TCErr -> ServerM a) -> ServerM a
catchTCError m h =
  ReaderT $ \env -> LSP.LspT $ ReaderT $ \lspEnv ->
    LSP.runLspT lspEnv (runServerT env m)
      `E.catch` \err -> LSP.runLspT lspEnv (runServerT env (h err))

-- | Provider
provideCommand :: (Monad m, MonadIO m) => IOTCM -> ServerT m ()
provideCommand iotcm = do
  controller <- asks envCommandController
  liftIO $ CommandController.put controller iotcm

-- | Consumer
consumeCommand :: (Monad m, MonadIO m) => Env -> m IOTCM
consumeCommand env = liftIO $ CommandController.take (envCommandController env)

waitUntilResponsesSent :: (Monad m, MonadIO m) => ServerT m ()
waitUntilResponsesSent = do
  controller <- asks envResponseController
  liftIO $ ResponseController.setCheckpointAndWait controller

signalCommandFinish :: (Monad m, MonadIO m) => ServerT m ()
signalCommandFinish = do
  writeLog "[Command] Finished"
  -- send `ResponseEnd`
  env <- ask
  liftIO $ writeChan (envResponseChan env) ResponseEnd
  -- allow the next Command to be consumed
  liftIO $ CommandController.release (envCommandController env)

-- | Sends a Response to the client via "envResponseChan"
sendResponse :: (Monad m, MonadIO m) => Env -> Response -> TCMT m ()
sendResponse env response = liftIO $ writeChan (envResponseChan env) response

--------------------------------------------------------------------------------

class (Monad m) => MonadMockLsp m where
  -- | When true, we're mocking LspM to the best of our ability. Usually, this
  -- should be false.
  --
  -- Used for testing purposes. It's hard to run @LspT@ without invoking the
  -- @lsp@ library, which means spinning up a server and no direct control, so
  -- in unit tests we hack around this with some @undefined@. To prevent this
  -- from being used, we can't access @LspT@ functionality, most notably
  -- logging. So, for example, this tells the logging implementation not to
  -- use LSP features.
  shouldMockLsp :: m Bool
  default shouldMockLsp :: (MonadTrans t, MonadMockLsp n, m ~ t n) => m Bool
  shouldMockLsp = lift shouldMockLsp

instance MonadMockLsp ServerM where
  shouldMockLsp = asks envMockLsp
