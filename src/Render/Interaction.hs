module Render.Interaction where

import qualified Data.Set as Set

import Agda.Syntax.Internal (Blocker(..))
import Agda.Interaction.Base
import Agda.TypeChecking.Monad
import Render.Class
import Render.Name ()
import Render.Position ()
import Render.RichText
import Render.TypeChecking ()

--------------------------------------------------------------------------------

-- | OutputForm
instance (Render a, Render b) => Render (OutputForm a b) where
  render (OutputForm r pids unblock c) = fsep [ prange r, parens (sep [blockedOn unblock, prPids pids]) ]
    where
      prPids [] = mempty
      prPids [pid] = parens $ "belongs to problem" <+> render pid
      prPids pids' = parens $ "belongs to problems" <+> fsep (punctuate "," $ fmap render pids')

      comma | null pids = mempty
            | otherwise = ","

      blockedOn (UnblockOnAll bs) | Set.null bs = mempty
      blockedOn (UnblockOnAny bs) | Set.null bs = "stuck" <> comma
      blockedOn u = undefined -- "blocked on" <+> (render u <> comma)

      prange rr
        | null s = mempty
        | otherwise = text $ " [ at " ++ s ++ " ]"
        where
          s = show $ render rr

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
  render (Assign name expr) =
    render name <> " := " <> render expr
  render (TypedAssign name expr1 expr2) =
    render name <> " := " <> render expr1 <> " :? " <> render expr2
  render (PostponedCheckArgs name exprs expr1 expr2) =
    let exprs' = fmap (parens . render) exprs
     in render name <> " := "
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
    let exprs' = (\(q, e, t) -> render q <> "=" <> render e <> " : " <> render t) <$> exprs
     in fsep
          [ "Resolve instance argument ",
            render name <> " : " <> render expr,
            "Candidate:",
            vcat exprs'
          ]
  render (PTSInstance name1 name2) =
    "PTS instance for ("
      <> render name1
      <> ", "
      <> render name2
      <> ")"
  render (PostponedCheckFunDef name expr _err) =
    "Check definition of "
      <> render name
      <> " : "
      <> render expr

-- | IPBoundary'
instance Render c => Render (IPBoundary' c) where
  render (IPBoundary eqs val meta over) = do
    let xs = fmap (\(l, r) -> render l <+> "=" <+> render r) eqs
        rhs = case over of
          Overapplied -> "=" <+> render meta
          NotOverapplied -> mempty
    fsep (punctuate "," xs) <+> "⊢" <+> render val <+> rhs
