{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Server.Model.Handler
  ( notificationHandlerWithAgdaLib,
    takeOverNotificationHandlerWithAgdaLib,
    requestHandlerWithAgdaFile,
  )
where

import Agda.Syntax.Common.Pretty (pretty, prettyShow)
import qualified Agda.TypeChecking.Monad as TCM
import qualified Agda.TypeChecking.Pretty as TCM
import Agda.Utils.Either (fromRightM)
import Agda.Utils.Lens ((^.))
import Control.Monad.Trans (lift)
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (ServerM, askModel, catchTCError)
import Server.AgdaProjectResolver (findAgdaProject)
import qualified Server.Log as Log
import qualified Server.Model as Model
import Server.Model.Monad (WithAgdaFileM, WithAgdaProjectM, runWithAgdaFileT, runWithAgdaProjectT)
#if MIN_VERSION_Agda(2,7,0)
#else
import Agda.TypeChecking.Errors ()
#endif

tryTC :: ServerM a -> ServerM (Either TCM.TCErr a)
tryTC handler = (Right <$> handler) `catchTCError` (return . Left)

--------------------------------------------------------------------------------

type NotificationHandlerWithAgdaLib
  (m :: LSP.Method LSP.ClientToServer LSP.Notification) =
  LSP.NormalizedUri -> LSP.TNotificationMessage m -> WithAgdaProjectM ()

notificationHandlerWithAgdaLib ::
  (LSP.HasTextDocument (LSP.MessageParams m) textdoc, LSP.HasUri textdoc LSP.Uri) =>
  LSP.SMethod m ->
  NotificationHandlerWithAgdaLib m ->
  LSP.Handlers ServerM
notificationHandlerWithAgdaLib m handlerWithAgdaLib =
  LSP.notificationHandler m $ flip takeOverNotificationHandlerWithAgdaLib handlerWithAgdaLib

takeOverNotificationHandlerWithAgdaLib ::
  (LSP.HasTextDocument (LSP.MessageParams m) textdoc, LSP.HasUri textdoc LSP.Uri) =>
  LSP.TNotificationMessage m ->
  NotificationHandlerWithAgdaLib m ->
  ServerM ()
takeOverNotificationHandlerWithAgdaLib notification handlerWithAgdaLib = do
  let uri = notification ^. LSP.params . LSP.textDocument . LSP.uri
      normUri = LSP.toNormalizedUri uri
  agdaProject <- findAgdaProject uri
  Log.infoP $ "For URI " <> pretty uri <> ", resolved project " <> pretty agdaProject

  let notificationHandler = runWithAgdaProjectT agdaProject . handlerWithAgdaLib normUri
  let handler = tryTC $ notificationHandler notification

  let onErr = \err -> runWithAgdaProjectT agdaProject $ Log.errorTCM err

  fromRightM onErr handler

--------------------------------------------------------------------------------

type RequestCallbackWithAgdaFile
  (m :: LSP.Method LSP.ClientToServer LSP.Request) =
  Either (LSP.TResponseError m) (LSP.MessageResult m) -> WithAgdaFileM ()

type RequestHandlerWithAgdaFile
  (m :: LSP.Method LSP.ClientToServer LSP.Request) =
  LSP.TRequestMessage m ->
  RequestCallbackWithAgdaFile m ->
  WithAgdaFileM ()

requestHandlerWithAgdaFile ::
  (LSP.HasTextDocument (LSP.MessageParams m) textdoc, LSP.HasUri textdoc LSP.Uri) =>
  LSP.SMethod m ->
  RequestHandlerWithAgdaFile m ->
  LSP.Handlers ServerM
requestHandlerWithAgdaFile m handlerWithAgdaFile = LSP.requestHandler m $ \req responder -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
      normUri = LSP.toNormalizedUri uri

  model <- askModel
  case Model.getAgdaFile normUri model of
    Nothing -> do
      let message = "Request for unknown Agda file at URI: " <> LSP.getUri uri
      responder $ Left $ LSP.TResponseError (LSP.InR LSP.ErrorCodes_InvalidParams) message Nothing
    Just agdaFile -> do
      agdaProject <- findAgdaProject uri
      let responderWithAgdaFile = lift . responder
      let handler = tryTC $ runWithAgdaFileT agdaProject agdaFile $ handlerWithAgdaFile req responderWithAgdaFile

      let onErr = \err -> runWithAgdaFileT agdaProject agdaFile $ do
            message <- Text.pack . prettyShow <$> TCM.liftTCM (TCM.prettyTCM err)
            lift $ responder $ Left $ LSP.TResponseError (LSP.InL LSP.LSPErrorCodes_RequestFailed) message Nothing

      fromRightM onErr handler
