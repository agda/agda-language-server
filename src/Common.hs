{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Agda.Interaction.Base (IOTCM)
import qualified Agda.Interaction.Response as Agda
import qualified Agda.Syntax.Common as Agda
import Agda.TypeChecking.Monad (TCMT)
import Control.Concurrent
import Control.Concurrent.Foreman (Foreman)
import qualified Control.Concurrent.Foreman as Foreman
import Control.Concurrent.Throttler (Throttler)
import qualified Control.Concurrent.Throttler as Throttler
import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)

--------------------------------------------------------------------------------

class FromAgda a b | a -> b where
  fromAgda :: a -> b

instance FromAgda Agda.InteractionId Int where
  fromAgda = Agda.interactionId

data GiveResult
  = GiveString String
  | GiveParen
  | GiveNoParen
  deriving (Generic)

instance FromAgda Agda.GiveResult GiveResult where
  fromAgda (Agda.Give_String s) = GiveString s
  fromAgda Agda.Give_Paren = GiveParen
  fromAgda Agda.Give_NoParen = GiveNoParen

instance ToJSON GiveResult

data DisplayInfo 
  = DisplayInfoTempGeneric String
  | DisplayInfoOthers1 String 
  | DisplayInfoOthers2
  deriving (Generic)

instance ToJSON DisplayInfo

-- reaction to command (IOCTM)
data Reaction
  -- non-last responses
  = ReactionHighlightingInfo String
  | ReactionDisplayInfo DisplayInfo
  | ReactionStatus Bool Bool
  | ReactionClearHighlightingTokenBased
  | ReactionClearHighlightingNotOnlyTokenBased
  | ReactionRunningInfo Int String
  | ReactionClearRunningInfo
  | ReactionDoneAborting
  | ReactionDoneExiting
  | ReactionGiveAction Int GiveResult
  | -- priority: 1
    ReactionInteractionPoints [Int]
  | -- priority: 2
    ReactionMakeCaseFunction [String]
  | ReactionMakeCaseExtendedLambda [String]
  | ReactionSolveAll [(Int, String)]
  | -- priority: 3
    ReactionJumpToError FilePath Int

  | ReactionEnd
  deriving (Generic)

instance ToJSON Reaction

--------------------------------------------------------------------------------

data Env = Env
  { envLogChan :: Chan Text,
    envCmdThrottler :: Throttler IOTCM,
    envReactionChan :: Chan Reaction,
    envReactionController :: Foreman,
    envDevMode :: Bool
  }

type ServerM' m = ReaderT Env m

type ServerM = ServerM' IO

createInitEnv :: Bool -> IO Env
createInitEnv devMode =
  Env <$> newChan
    <*> Throttler.new
    <*> newChan
    <*> Foreman.new
    <*> pure devMode

runServerLSP :: Env -> LanguageContextEnv () -> LspT () (ServerM' m) a -> m a
runServerLSP env ctxEnv program = runReaderT (runLspT ctxEnv program) env

writeLog :: (Monad m, MonadIO m) => Text -> ServerM' m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

-- | Provider
provideCommand :: (Monad m, MonadIO m) => IOTCM -> ServerM' m ()
provideCommand iotcm = do
  throttler <- asks envCmdThrottler
  liftIO $ Throttler.put throttler iotcm

-- | Consumter
consumeCommand :: (Monad m, MonadIO m) => Env -> m IOTCM
consumeCommand env = liftIO $ Throttler.take (envCmdThrottler env)

waitUntilResponsesSent :: (Monad m, MonadIO m) => ServerM' m ()
waitUntilResponsesSent = do
  foreman <- asks envReactionController
  liftIO $ Foreman.setGoalAndWait foreman

signalCommandFinish :: (Monad m, MonadIO m) => ServerM' m ()
signalCommandFinish = do
  writeLog "[Command] Finished"
  -- send `ReactionEnd`
  env <- ask
  liftIO $ writeChan (envReactionChan env) ReactionEnd
  -- allow the next Command to be consumed
  liftIO $ Throttler.move (envCmdThrottler env)

sendReaction :: (Monad m, MonadIO m) => Env -> Reaction -> TCMT m ()
sendReaction env reaction = do
  liftIO $ writeChan (envReactionChan env) reaction
