{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Intermediate Representation for Agda's types
module Agda.IR where

import qualified Agda.Interaction.Base as Agda
import qualified Agda.Interaction.Response as Agda
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Common as Agda
import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Fixity as Agda
import qualified Agda.Syntax.Position as Agda
import qualified Agda.Syntax.Translation.AbstractToConcrete as Agda
import Agda.TypeChecking.Monad (TCM)
import qualified Agda.TypeChecking.Monad.Base as Agda
import qualified Agda.Utils.FileName as Agda
import qualified Agda.Utils.Pretty as Agda
import Data.Aeson
import qualified Data.Strict.Maybe as Strict
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Typeclass for converting Agda values into IR
class FromAgda a b | a -> b where
  fromAgda :: a -> b

class FromAgdaTCM a b | a -> b where
  fromAgdaTCM :: a -> TCM b

--------------------------------------------------------------------------------

-- | ToJSON instances for Agda types
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

-- | NamedMeta or InteractionId
data NMII
  = NamedMeta String Int
  | InteractionId Int
  deriving (Generic)

instance ToJSON NMII

instance FromAgda Agda.NamedMeta NMII where
  fromAgda (Agda.NamedMeta name (Agda.MetaId i)) = NamedMeta name i

instance FromAgda Agda.InteractionId NMII where
  fromAgda (Agda.InteractionId i) = InteractionId i

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | Expression

-- convert A.Expr ==> C.Expr ==> String
instance FromAgdaTCM A.Expr String where
  fromAgdaTCM expr = do
    expr' <- Agda.abstractToConcreteCtx Agda.TopCtx expr :: TCM C.Expr
    return $ show (Agda.pretty expr')

--------------------------------------------------------------------------------

-- | Comparison
type Comparison = Bool -- True for CmpEq

instance FromAgda Agda.Comparison Bool where
  fromAgda Agda.CmpEq = True
  fromAgda _ = False

--------------------------------------------------------------------------------

-- | IR for OutputConstraint
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

instance (FromAgda b NMII) => FromAgdaTCM (Agda.OutputConstraint A.Expr b) OutputConstraint where
  fromAgdaTCM (Agda.OfType name expr) =
    OfType (fromAgda name) <$> fromAgdaTCM expr
  fromAgdaTCM (Agda.JustType name) =
    return $
      JustType (fromAgda name)
  fromAgdaTCM (Agda.JustSort name) =
    return $
      JustSort (fromAgda name)
  fromAgdaTCM (Agda.CmpInType cmp expr name1 name2) =
    CmpInType (fromAgda cmp)
      <$> fromAgdaTCM expr <*> pure (fromAgda name1) <*> pure (fromAgda name2)
  fromAgdaTCM (Agda.CmpElim pols expr names1 names2) =
    CmpElim pols <$> fromAgdaTCM expr <*> pure (map fromAgda names1) <*> pure (map fromAgda names2)
  fromAgdaTCM (Agda.CmpTypes cmp name1 name2) =
    return $
      CmpTypes (fromAgda cmp) (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.CmpLevels cmp name1 name2) =
    return $
      CmpLevels (fromAgda cmp) (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.CmpTeles cmp name1 name2) =
    return $
      CmpTeles (fromAgda cmp) (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.CmpSorts cmp name1 name2) =
    return $
      CmpSorts (fromAgda cmp) (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.Guard x (Agda.ProblemId i)) =
    Guard <$> fromAgdaTCM x <*> pure i
  fromAgdaTCM (Agda.Assign name expr) =
    Assign (fromAgda name) <$> fromAgdaTCM expr
  fromAgdaTCM (Agda.TypedAssign name expr1 expr2) =
    TypedAssign (fromAgda name) <$> fromAgdaTCM expr1 <*> fromAgdaTCM expr2
  fromAgdaTCM (Agda.PostponedCheckArgs name exprs expr1 expr2) =
    PostponedCheckArgs (fromAgda name) <$> mapM fromAgdaTCM exprs <*> fromAgdaTCM expr1 <*> fromAgdaTCM expr2
  fromAgdaTCM (Agda.IsEmptyType expr) =
    IsEmptyType <$> fromAgdaTCM expr
  fromAgdaTCM (Agda.SizeLtSat expr) =
    SizeLtSat <$> fromAgdaTCM expr
  fromAgdaTCM (Agda.FindInstanceOF name expr exprs) =
    FindInstanceOF (fromAgda name) <$> fromAgdaTCM expr <*> mapM (\(a, b) -> (,) <$> fromAgdaTCM a <*> fromAgdaTCM b) exprs
  fromAgdaTCM (Agda.PTSInstance name1 name2) =
    return $
      PTSInstance (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.PostponedCheckFunDef name expr) =
    PostponedCheckFunDef (show (Agda.pretty name)) <$> fromAgdaTCM expr

--------------------------------------------------------------------------------

-- | IR for DisplayInfo
data DisplayInfo
  = DisplayInfoGeneric String String
  | DisplayInfoAllGoalsWarnings String [(OutputConstraint, String)] [(OutputConstraint, String, Agda.Range)] [String] [String]
  | DisplayInfoCompilationOk [String] [String]
  | DisplayInfoAuto String
  | DisplayInfoError String
  | DisplayInfoTime String
  | DisplayInfoNormalForm String
  deriving (Generic)

instance ToJSON DisplayInfo

--------------------------------------------------------------------------------
-- IR for IOCTM
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