{-# LANGUAGE OverloadedStrings #-}

module Render.Interaction where

import Render.Class
import Render.Name ()
import Render.RichText
import Agda.Interaction.Base
import Agda.TypeChecking.Monad


--------------------------------------------------------------------------------

-- | OutputConstraint 
instance (RenderTCM a, Render b) => RenderTCM (OutputConstraint a b) where
  renderTCM (OfType name expr) =
    renderA name <> " : " <> renderTCM expr
  renderTCM (JustType name) =
    "Type " <> renderA name
  renderTCM (JustSort name) =
    "Sort " <> renderA name
  renderTCM (CmpInType cmp expr name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
      <> " : "
      <> renderTCM expr
  renderTCM (CmpElim pols expr names1 names2) =
    renderA names1
      <> " "
      <> renderP pols
      <> " "
      <> renderA names2
      <> " : "
      <> renderTCM expr
  renderTCM (CmpTypes cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (CmpLevels cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (CmpTeles cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (CmpSorts cmp name1 name2) =
    renderA name1
      <> " "
      <> renderP cmp
      <> " "
      <> renderA name2
  renderTCM (Guard x (ProblemId pid)) =
    renderTCM x
      <> indentM (parensM ("blocked by problem " <> renderP pid))
  renderTCM (Assign name expr) =
    renderA name <> " := " <> renderTCM expr
  renderTCM (TypedAssign name expr1 expr2) =
    renderA name <> " := " <> renderTCM expr1 <> " :? " <> renderTCM expr2
  renderTCM (PostponedCheckArgs name exprs expr1 expr2) = do
    exprs' <- mapM (parensM <$> renderTCM) exprs
    renderA name <> " := "
      <> parensM ("_ : " <> renderTCM expr1)
      <> " "
      <> vsepM exprs'
      <> " : "
      <> renderTCM expr2
  renderTCM (IsEmptyType expr) =
    "Is empty: " <> renderTCM expr
  renderTCM (SizeLtSat expr) =
    "Not empty type of sizes: " <> renderTCM expr
  renderTCM (FindInstanceOF name expr exprs) = do
    exprs' <- mapM (\(e, t) -> renderTCM e <> " : " <> renderTCM t) exprs
    "Resolve instance argument "
      <> indentM (renderA name <> " : " <> renderTCM expr)
      <> indentM "Candidate:"
      <> indentM (indentM (vsepM exprs'))
  renderTCM (PTSInstance name1 name2) =
    "PTS instance for ("
      <> renderA name1
      <> ", "
      <> renderA name2
      <> ")"
  renderTCM (PostponedCheckFunDef name expr) =
    "Check definition of "
      <> renderA name
      <> " : "
      <> renderTCM expr
