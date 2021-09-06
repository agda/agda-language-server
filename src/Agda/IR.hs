{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Intermediate Representation for Agda's types
module Agda.IR where

import qualified Agda.Interaction.Response as Agda
import Agda.TypeChecking.Monad (TCM)
import Data.Aeson
import GHC.Generics (Generic)
import Render
import Agda.Syntax.Position (Range)

--------------------------------------------------------------------------------

-- | Typeclass for converting Agda values into IR
class FromAgda a b | a -> b where
  fromAgda :: a -> b

class FromAgdaTCM a b | a -> b where
  fromAgdaTCM :: a -> TCM b

--------------------------------------------------------------------------------
-- | IR for IOCTM
data Response
  = -- non-last responses
    ResponseHighlightingInfoDirect HighlightingInfos
  | ResponseHighlightingInfoIndirect FilePath
  | ResponseDisplayInfo DisplayInfo
  | ResponseStatus Bool Bool
  | ResponseClearHighlightingTokenBased
  | ResponseClearHighlightingNotOnlyTokenBased
  | ResponseRunningInfo Int String
  | ResponseClearRunningInfo
  | ResponseDoneAborting
  | ResponseDoneExiting
  | ResponseGiveAction Int GiveResult
  | -- priority: 1
    ResponseInteractionPoints [Int]
  | -- priority: 2
    ResponseMakeCaseFunction [String]
  | ResponseMakeCaseExtendedLambda [String]
  | ResponseSolveAll [(Int, String)]
  | -- priority: 3
    ResponseJumpToError FilePath Int
  | ResponseEnd
  deriving (Generic)

instance ToJSON Response

--------------------------------------------------------------------------------

-- | IR for DisplayInfo
data DisplayInfo
  = DisplayInfoGeneric String [Block]
  | DisplayInfoAllGoalsWarnings String [Block] [Block] [String] [String]
  | DisplayInfoCurrentGoal Block
  | DisplayInfoInferredType Block
  | DisplayInfoCompilationOk [String] [String]
  | DisplayInfoAuto String
  | DisplayInfoError String
  | DisplayInfoTime String
  | DisplayInfoNormalForm String
  deriving (Generic)

instance ToJSON DisplayInfo

--------------------------------------------------------------------------------

-- | GiveResult
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

--------------------------------------------------------------------------------

-- | IR for HighlightingInfo
data HighlightingInfo
  = HighlightingInfo
      Int -- starting offset
      Int -- ending offset
      [String] -- list of names of aspects
      Bool -- is token based?
      String -- note
      (Maybe (FilePath, Int)) -- the defining module of the token and its position in that module
  deriving (Generic, Show)

instance ToJSON HighlightingInfo

data HighlightingInfos = HighlightingInfos Bool [HighlightingInfo]
  deriving (Generic, Show)

instance ToJSON HighlightingInfos
