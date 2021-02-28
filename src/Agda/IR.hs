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
import RichText

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
  | DisplayInfoAllGoalsWarnings String [(RichText, String)] [(RichText, String, Agda.Range)] [String] [String]
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
  deriving (Generic, Show)

instance ToJSON NamedMeta

instance FromAgda Agda.NamedMeta NamedMeta where
  fromAgda (Agda.NamedMeta name (Agda.MetaId i)) = NamedMeta name i

instance Render NamedMeta where
  render (NamedMeta "" x) = render x
  render (NamedMeta "_" x) = render x
  render (NamedMeta s x) = "_" <> string s <> render x

instance Render Agda.NamedMeta where
  render (Agda.NamedMeta "" (Agda.MetaId x)) = render x
  render (Agda.NamedMeta "_" (Agda.MetaId x)) = render x
  render (Agda.NamedMeta s (Agda.MetaId x)) = "_" <> string s <> render x

--------------------------------------------------------------------------------

newtype InteractionId
  = InteractionId Int
  deriving (Generic, Show)

instance ToJSON InteractionId

instance FromAgda Agda.InteractionId InteractionId where
  fromAgda (Agda.InteractionId i) = InteractionId i

instance Render InteractionId where
  render (InteractionId i) = link (Hole i) ("?" <> render i)

instance Render Agda.InteractionId where
  render (Agda.InteractionId i) = link (Hole i) ("?" <> render i)

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
  deriving (Generic, Show)

instance ToJSON HighlightingInfo

data HighlightingInfos = HighlightingInfos Bool [HighlightingInfo]
  deriving (Generic, Show)

instance ToJSON HighlightingInfos

--------------------------------------------------------------------------------

-- | Expression

-- convert A.Expr ==> C.Expr ==> String
instance FromAgdaTCM A.Expr String where
  fromAgdaTCM expr = do
    expr' <- Agda.abstractToConcreteCtx Agda.TopCtx expr :: TCM C.Expr
    return $ show (Agda.pretty expr')

-- convert A.Expr ==> C.Expr ==> RichText
instance RenderTCM A.Expr where
  renderTCM expr = do
    expr' <- Agda.abstractToConcreteCtx Agda.TopCtx expr :: TCM C.Expr
    return $ string $ show (Agda.pretty expr')

instance Render C.NamePart where
  render C.Hole = "_"
  render (C.Id s) = string $ Agda.rawNameToString s

instance Render C.Name where
  render (C.Name _ _ xs) = sepBy' "." (map render xs)
  render (C.NoName _ _) = "_"

instance Render A.Name where
  render = render . A.nameConcrete

instance Render A.QName where
  render (A.QName m x) = sepBy' "." (map render (A.mnameToList m ++ [x]))

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

instance (RenderTCM a, Render b) => RenderTCM (Agda.OutputConstraint a b) where
  renderTCM (Agda.OfType name expr) =
    renderA name <> " : " <> renderTCM expr
  renderTCM (Agda.JustType name) =
    "Type " <> renderA name
  renderTCM (Agda.JustSort name) =
    "Sort " <> renderA name
  renderTCM (Agda.CmpInType cmp expr name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
      <> " : "
      <> renderTCM expr
  renderTCM (Agda.CmpElim pols expr names1 names2) =
    renderA names1
      <> " "
      <> renderP pols
      <> " "
      <> renderA names2
      <> " : "
      <> renderTCM expr
  renderTCM (Agda.CmpTypes cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (Agda.CmpLevels cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (Agda.CmpTeles cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (Agda.CmpSorts cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (Agda.Guard x (Agda.ProblemId pid)) =
    renderTCM x
      <> indent (parens ("blocked by problem " <> renderP pid))
  renderTCM (Agda.Assign name expr) =
    renderA name <> " := " <> renderTCM expr
  renderTCM (Agda.TypedAssign name expr1 expr2) =
    renderA name <> " := " <> renderTCM expr1 <> " :? " <> renderTCM expr2
  renderTCM (Agda.PostponedCheckArgs name exprs expr1 expr2) = do
    exprs' <- mapM (parens <$> renderTCM) exprs
    renderA name <> " := "
      <> parens ("_ : " <> renderTCM expr1)
      <> " "
      <> vsep exprs'
      <> " : "
      <> renderTCM expr2
  renderTCM (Agda.IsEmptyType expr) =
    "Is empty: " <> renderTCM expr
  renderTCM (Agda.SizeLtSat expr) =
    "Not empty type of sizes: " <> renderTCM expr
  renderTCM (Agda.FindInstanceOF name expr exprs) = do
    exprs' <- mapM (\(e, t) -> renderTCM e <> " : " <> renderTCM t) exprs
    "Resolve instance argument "
      <> indent (renderA name <> " : " <> renderTCM expr)
      <> indent "Candidate:"
      <> indent (indent (vsep exprs'))
  renderTCM (Agda.PTSInstance name1 name2) =
    "PTS instance for ("
      <> renderA name1
      <> ", "
      <> renderA name2
      <> ")"
  renderTCM (Agda.PostponedCheckFunDef name expr) =
    "Check definition of "
      <> renderA name
      <> " : "
      <> renderTCM expr

-- "Check definition of" <+> pretty q <+> ":" <+> pretty a
-- PostponedCheckFunDef (show (Agda.pretty name)) <$> fromAgdaTCM expr

--------------------------------------------------------------------------------

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