{-# LANGUAGE OverloadedStrings #-}

module Render.Interaction where

import Render.Class
import Render.Name ()
import Render.TypeChecking ()
import Render.Position ()
import Render.RichText
import Agda.Interaction.Base
import Agda.TypeChecking.Monad


--------------------------------------------------------------------------------

-- | OutputForm 
instance (Render a, Render b) => Render (OutputForm a b) where
  render (OutputForm r pids c) = sep [render c, indent $ prange r, indent $ prPids pids]
    where
      prPids []    = mempty
      prPids [pid] = parens $ "problem" <+> render pid
      prPids pids' = parens $ "problems" <+> fsep (punctuate "," $ map render pids)
      prange rr | null s = mempty
                | otherwise = text $ " [ at " ++ s ++ " ]"
        where s = show $ render rr

-- | OutputConstraint 
instance (Render a, Render b) => Render (OutputConstraint a b) where
  render (OfType name expr) =
    render name <> " : " <> render expr
  render (JustType name) =
    "Type " <> render name
  render (JustSort name) =
    "Sort " <> render name
  render (CmpInType cmp expr name1 name2) =
    render name1
      <> " "
      <> render cmp
      <> " "
      <> render name2
      <> " : "
      <> render expr
  render (CmpElim pols expr names1 names2) =
    render names1
      <> " "
      <> render pols
      <> " "
      <> render names2
      <> " : "
      <> render expr
  render (CmpTypes cmp name1 name2) =
    render name1
      <> " "
      <> render cmp
      <> " "
      <> render name2
  render (CmpLevels cmp name1 name2) =
    render name1
      <> " "
      <> render cmp
      <> " "
      <> render name2
  render (CmpTeles cmp name1 name2) =
    render name1
      <> " "
      <> render cmp
      <> " "
      <> render name2
  render (CmpSorts cmp name1 name2) =
    render name1
      <> " "
      <> render cmp
      <> " "
      <> render name2
  render (Guard x (ProblemId pid)) =
    render x
      <> indent (parens ("blocked by problem " <> render pid))
  render (Assign name expr) =
    render name <> " := " <> render expr
  render (TypedAssign name expr1 expr2) =
    render name <> " := " <> render expr1 <> " :? " <> render expr2
  render (PostponedCheckArgs name exprs expr1 expr2) = 
    let exprs' = map (parens . render) exprs in
    render name <> " := "
      <> parens ("_ : " <> render expr1)
      <> " "
      <> fsep exprs'
      <> " : "
      <> render expr2
  render (IsEmptyType expr) =
    "Is empty: " <> render expr
  render (SizeLtSat expr) =
    "Not empty type of sizes: " <> render expr
  render (FindInstanceOF name expr exprs) = 
    let exprs' = map (\(e, t) -> render e <> " : " <> render t) exprs in
    "Resolve instance argument "
      <> indent (render name <> " : " <> render expr)
      <> indent "Candidate:"
      <> indent (indent (vcat exprs'))
  render (PTSInstance name1 name2) =
    "PTS instance for ("
      <> render name1
      <> ", "
      <> render name2
      <> ")"
  render (PostponedCheckFunDef name expr) =
    "Check definition of "
      <> render name
      <> " : "
      <> render expr

-- | IPBoundary'
instance Render c => Render (IPBoundary' c) where
  render (IPBoundary eqs val meta over) = do
    let
      xs = map (\ (l,r) -> render l <+> "=" <+> render r) eqs
      rhs = case over of
              Overapplied    -> "=" <+> render meta
              NotOverapplied -> mempty
    fsep (punctuate "," xs) <+> "⊢" <+> render val <+> rhs
