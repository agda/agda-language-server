{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

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

--------------------------------------------------------------------------------

-- | IR for DisplayInfo
data DisplayInfo
  = DisplayInfoGeneric String String
  | DisplayInfoAllGoalsWarnings String [(OutputConstraint InteractionId, String)] [(OutputConstraint NamedMeta, String, Agda.Range)] [String] [String]
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

-- | NamedMeta
data NamedMeta
  = NamedMeta String Int
  deriving (Generic)

instance ToJSON NamedMeta

instance FromAgda Agda.NamedMeta NamedMeta where
  fromAgda (Agda.NamedMeta name (Agda.MetaId i)) = NamedMeta name i

--------------------------------------------------------------------------------

newtype InteractionId
  = InteractionId Int
  deriving (Generic)

instance ToJSON InteractionId

instance FromAgda Agda.InteractionId InteractionId where
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
--------------------------------------------------------------------------------

-- | IR for OutputConstraint
data OutputConstraint b
  = OfType b String
  | JustType b
  | JustSort b
  | CmpInType Agda.Comparison String b b
  | CmpElim [Agda.Polarity] String [b] [b]
  | CmpTypes Agda.Comparison b b
  | CmpLevels Agda.Comparison b b
  | CmpTeles Agda.Comparison b b
  | CmpSorts Agda.Comparison b b
  | Guard (OutputConstraint b) Int
  | Assign b String
  | TypedAssign b String String
  | PostponedCheckArgs b [String] String String
  | IsEmptyType String
  | SizeLtSat String
  | FindInstanceOF b String [(String, String)]
  | PTSInstance b b
  | PostponedCheckFunDef String String
  deriving (Generic)

instance ToJSON b => ToJSON (OutputConstraint b)

instance (FromAgda c d) => FromAgdaTCM (Agda.OutputConstraint A.Expr c) (OutputConstraint d) where
  fromAgdaTCM (Agda.OfType name expr) =
    OfType (fromAgda name) <$> fromAgdaTCM expr
  fromAgdaTCM (Agda.JustType name) =
    return $
      JustType (fromAgda name)
  fromAgdaTCM (Agda.JustSort name) =
    return $
      JustSort (fromAgda name)
  fromAgdaTCM (Agda.CmpInType cmp expr name1 name2) =
    CmpInType cmp
      <$> fromAgdaTCM expr <*> pure (fromAgda name1) <*> pure (fromAgda name2)
  fromAgdaTCM (Agda.CmpElim pols expr names1 names2) =
    CmpElim pols <$> fromAgdaTCM expr <*> pure (map fromAgda names1) <*> pure (map fromAgda names2)
  fromAgdaTCM (Agda.CmpTypes cmp name1 name2) =
    return $
      CmpTypes cmp (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.CmpLevels cmp name1 name2) =
    return $
      CmpLevels cmp (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.CmpTeles cmp name1 name2) =
    return $
      CmpTeles cmp (fromAgda name1) (fromAgda name2)
  fromAgdaTCM (Agda.CmpSorts cmp name1 name2) =
    return $
      CmpSorts cmp (fromAgda name1) (fromAgda name2)
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

-- | ToJSON for Agda.TypeChecking.Monad.Base.Comparison
instance ToJSON Agda.Comparison where
  toJSON Agda.CmpEq = String "Covariant"
  toJSON Agda.CmpLeq = String "Contravariant"