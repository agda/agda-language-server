module Server.Handler.TextDocument.FileManagement
  ( didOpenHandler,
    didCloseHandler,
    didSaveHandler,
  )
where

import Agda.Interaction.Imports.Virtual (parseVSource, vSrcFromUri)
import Agda.Syntax.Common.Pretty (pretty, (<+>))
import Agda.Utils.Lens ((^.))
import Control.Monad.Trans (lift)
import Indexer (indexFile)
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (ServerM, modifyModel, modifyVfsIndex)
import qualified Server.Log as Log
import qualified Server.Model as Model
import Server.Model.Handler (notificationHandlerWithAgdaLib, takeOverNotificationHandlerWithAgdaLib)
import qualified Server.VfsIndex as VfsIndex

didOpenHandler :: LSP.Handlers ServerM
didOpenHandler = LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen $ \notification -> do
  let uri = notification ^. LSP.params . LSP.textDocument . LSP.uri
  Log.infoP $ "Opening" <+> pretty uri
  modifyVfsIndex $ VfsIndex.onOpen uri
  takeOverNotificationHandlerWithAgdaLib notification $ \uri notification -> do
    vfile <- lift $ LSP.getVirtualFile uri
    case vfile of
      Nothing -> do
        Log.warnT "Failed to open: could not find file"
      Just vfile -> do
        vSourceFile <- vSrcFromUri uri vfile
        src <- parseVSource vSourceFile
        Log.infoP $ "Opening source" <+> pretty src
        agdaFile <- indexFile src
        lift $ modifyModel $ Model.setAgdaFile uri agdaFile

didCloseHandler :: LSP.Handlers ServerM
didCloseHandler = LSP.notificationHandler LSP.SMethod_TextDocumentDidClose $ \notification -> do
  let uri = notification ^. LSP.params . LSP.textDocument . LSP.uri
  Log.infoP $ "Closing URI" <+> pretty uri
  modifyVfsIndex $ VfsIndex.onClose uri
  modifyModel $ Model.deleteAgdaFile $ LSP.toNormalizedUri uri

didSaveHandler :: LSP.Handlers ServerM
didSaveHandler = notificationHandlerWithAgdaLib LSP.SMethod_TextDocumentDidSave $ \uri notification -> do
  Log.infoP $ "Saving URI" <+> pretty uri
  vfile <- lift $ LSP.getVirtualFile uri
  case vfile of
    Nothing -> return ()
    Just vfile -> do
      vSourceFile <- vSrcFromUri uri vfile
      src <- parseVSource vSourceFile
      Log.infoP $ "Saving source" <+> pretty src
      agdaFile <- indexFile src
      lift $ modifyModel $ Model.setAgdaFile uri agdaFile
