{-# LANGUAGE FlexibleContexts #-}

module Server.Log
  ( traceT,
    traceP,
    infoT,
    infoP,
    warnT,
    warnP,
    errorT,
    errorP,
    errorTCM,
  )
where

import Agda.Syntax.Common.Pretty (Pretty, prettyShow)
import Agda.TypeChecking.Monad (MonadTCM, liftTCM)
import Agda.TypeChecking.Pretty (PrettyTCM, prettyTCM)
import Agda.Utils.Monad (ifM, (<=<))
import Colog.Core (Severity (Error, Info, Warning), WithSeverity (WithSeverity), (<&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Logging as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server (MonadLsp)
import qualified Language.LSP.Server as LSP
import Monad (MonadMockLsp (shouldMockLsp))
import Options (Config)

trace :: (MonadLsp Config m, MonadMockLsp m) => Text -> m ()
trace text =
  ifM shouldMockLsp (pure ()) $
    LSP.sendNotification LSP.SMethod_LogTrace $
      LSP.LogTraceParams text Nothing

console :: (MonadLsp Config m, MonadMockLsp m) => Severity -> Text -> m ()
console severity text =
  ifM shouldMockLsp (pure ()) $
    (LSP.logToLogMessage <& WithSeverity text severity)

popup :: (MonadLsp Config m, MonadMockLsp m) => Severity -> Text -> m ()
popup severity text =
  ifM shouldMockLsp (pure ()) $
    LSP.logToShowMessage <& WithSeverity text severity

traceT :: (MonadLsp Config m, MonadMockLsp m) => Text -> m ()
traceT text = trace text

traceP :: (MonadLsp Config m, MonadMockLsp m, Pretty p) => p -> m ()
traceP = traceT . Text.pack . prettyShow

infoT :: (MonadLsp Config m, MonadMockLsp m) => Text -> m ()
infoT text = console Info text

infoP :: (MonadLsp Config m, MonadMockLsp m, Pretty p) => p -> m ()
infoP = infoT . Text.pack . prettyShow

warnT :: (MonadLsp Config m, MonadMockLsp m) => Text -> m ()
warnT text = console Warning text

warnP :: (MonadLsp Config m, MonadMockLsp m, Pretty p) => p -> m ()
warnP = warnT . Text.pack . prettyShow

errorT :: (MonadLsp Config m, MonadMockLsp m) => Text -> m ()
errorT text = console Error text >> popup Error text

errorP :: (MonadLsp Config m, MonadMockLsp m, Pretty p) => p -> m ()
errorP = errorT . Text.pack . prettyShow

errorTCM :: (MonadLsp Config m, MonadMockLsp m, MonadTCM m, PrettyTCM p) => p -> m ()
errorTCM = errorP <=< liftTCM . prettyTCM
