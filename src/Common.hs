{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Agda.Interaction.Base (IOTCM)
import qualified Agda.Interaction.Response as Agda
import qualified Agda.Syntax.Common as Agda
import qualified Agda.Syntax.Position as Agda
import Agda.TypeChecking.Monad (TCMT)
import qualified Agda.TypeChecking.Monad.Base as Agda
import qualified Agda.Utils.FileName as Agda
import Control.Concurrent
import Control.Concurrent.Foreman (Foreman)
import qualified Control.Concurrent.Foreman as Foreman
import Control.Concurrent.Throttler (Throttler)
import qualified Control.Concurrent.Throttler as Throttler
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Encoding as JSON
import Data.Aeson.Types (Parser)
import qualified Data.Strict.Maybe as Strict
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)

          -- fromOutputConstraint :: OutputConstraint A.Expr NamedMeta -> TCM IR.OutputConstraint
          -- fromOutputConstraint (OfType name expr) =
          --   IR.OfType (namedMeta name) <$> fromExpr expr
          -- fromOutputConstraint (JustType name) =
          --   return $
          --     IR.JustType (namedMeta name)
          -- fromOutputConstraint (JustSort name) =
          --   return $
          --     IR.JustSort (namedMeta name)
          -- fromOutputConstraint (CmpInType cmp expr name1 name2) =
          --   IR.CmpInType (fromCmp cmp)
          --     <$> fromExpr expr <*> pure (namedMeta name1) <*> pure (namedMeta name1)
          -- fromOutputConstraint (CmpElim pols expr names1 names2) =
          --   IR.CmpElim pols <$> fromExpr expr <*> pure (map namedMeta names1) <*> pure (map namedMeta names2)
          -- fromOutputConstraint (CmpTypes cmp name1 name2) =
          --   return $
          --     IR.CmpTypes (fromCmp cmp) (namedMeta name1) (namedMeta name1)
          -- fromOutputConstraint (CmpLevels cmp name1 name2) =
          --   return $
          --     IR.CmpLevels (fromCmp cmp) (namedMeta name1) (namedMeta name1)
          -- fromOutputConstraint (CmpTeles cmp name1 name2) =
          --   return $
          --     IR.CmpTeles (fromCmp cmp) (namedMeta name1) (namedMeta name1)
          -- fromOutputConstraint (CmpSorts cmp name1 name2) =
          --   return $
          --     IR.CmpSorts (fromCmp cmp) (namedMeta name1) (namedMeta name1)
          -- fromOutputConstraint (Guard x (ProblemId i)) =
          --   IR.Guard <$> fromOutputConstraint x <*> pure i
          -- fromOutputConstraint (Assign name expr) =
          --   IR.Assign (namedMeta name) <$> fromExpr expr
          -- fromOutputConstraint (TypedAssign name expr1 expr2) =
          --   IR.TypedAssign (namedMeta name) <$> fromExpr expr1 <*> fromExpr expr2
          -- fromOutputConstraint (PostponedCheckArgs name exprs expr1 expr2) =
          --   IR.PostponedCheckArgs (namedMeta name) <$> mapM fromExpr exprs <*> fromExpr expr1 <*> fromExpr expr2
          -- fromOutputConstraint (IsEmptyType expr) =
          --   IR.IsEmptyType <$> fromExpr expr
          -- fromOutputConstraint (SizeLtSat expr) =
          --   IR.SizeLtSat <$> fromExpr expr
          -- fromOutputConstraint (FindInstanceOF name expr exprs) =
          --   IR.FindInstanceOF (namedMeta name) <$> fromExpr expr <*> mapM (\(a, b) -> (,) <$> fromExpr a <*> fromExpr b) exprs
          -- fromOutputConstraint (PTSInstance name1 name2) =
          --   return $
          --     IR.PTSInstance (namedMeta name1) (namedMeta name1)
          -- fromOutputConstraint (PostponedCheckFunDef qname expr) =
          --     IR.PostponedCheckFunDef (show (pretty qname)) <$> fromExpr expr 

--------------------------------------------------------------------------------

-- | Typeclass for converting Agda values into IR
class FromAgda a b | a -> b where
  fromAgda :: a -> b

class FromAgdaTCM a b | a -> b where
  fromAgdaTCM :: a -> Agda.TCM b 

-- | IR for GiveResult
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

-- | IR for DisplayInfo
data DisplayInfo
  = DisplayInfoGeneric String String
  | DisplayInfoAllGoalsWarnings String [(OutputConstraint, String)] [(OutputConstraint, String, Agda.Range)] [String] [String]
  | DisplayInfoCompilationOk String
  | DisplayInfoAuto String
  | DisplayInfoError String
  | DisplayInfoTime String
  | DisplayInfoNormalForm String
  deriving (Generic)

instance ToJSON DisplayInfo

-- | NamedMeta or InteractionId
data NMII
  = NamedMeta String Int
  | InteractionId Int
  deriving (Generic)

instance ToJSON NMII

-- | IR for OutputConstraint
type Comparison = Bool -- True for CmpEq

data OutputConstraint
  = OfType NMII String
  | JustType NMII
  | JustSort NMII
  | CmpInType Comparison String NMII NMII
  | CmpElim [Agda.Polarity] String [NMII] [NMII]
  | CmpTypes Comparison NMII NMII
  | CmpLevels Comparison NMII NMII
  | CmpTeles Comparison NMII NMII
  | CmpSorts Comparison NMII NMII
  | Guard OutputConstraint Int
  | Assign NMII String
  | TypedAssign NMII String String
  | PostponedCheckArgs NMII [String] String String
  | IsEmptyType String
  | SizeLtSat String
  | FindInstanceOF NMII String [(String, String)]
  | PTSInstance NMII NMII
  | PostponedCheckFunDef String String
  deriving (Generic)

instance ToJSON OutputConstraint

-- | IR for HighlightingInfo
data HighlightingInfo
  = HighlightingInfo
      Int -- starting offset
      Int -- ending offset
      [String] -- list of names of aspects
      Bool -- is token based?
      (Maybe String) -- note
      (Maybe (FilePath, Int)) -- the defining module of the token and its position in that module
  deriving (Generic)

instance ToJSON HighlightingInfo

data HighlightingInfos = HighlightingInfos Bool [HighlightingInfo]
  deriving (Generic)

instance ToJSON HighlightingInfos

-- reaction to command (IOCTM)
data Reaction
  = -- non-last responses
    ReactionHighlightingInfoDirect HighlightingInfos
  | ReactionHighlightingInfoIndirect FilePath
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

-- | ToJSON for Agda.Syntax.Position.Range
instance ToJSON Agda.Range

instance ToJSON (Agda.Interval' ()) where
  toJSON (Agda.Interval start end) = toJSON (start, end)

instance ToJSON (Agda.Position' ()) where
  toJSON (Agda.Pn () pos line col) = toJSON [line, col, pos]

instance ToJSON Agda.SrcFile where
  toJSON Strict.Nothing = Null
  toJSON (Strict.Just path) = toJSON path

instance ToJSON Agda.AbsolutePath where
  toJSON (Agda.AbsolutePath path) = toJSON path

-- | ToJSON for Agda.TypeChecking.Monad.Base.Polarity
instance ToJSON Agda.Polarity where
  toJSON Agda.Covariant = String "Covariant"
  toJSON Agda.Contravariant = String "Contravariant"
  toJSON Agda.Invariant = String "Invariant"
  toJSON Agda.Nonvariant = String "Nonvariant"

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
