{-# LANGUAGE CPP #-}

module Render.Common where

import Agda.Syntax.Common
  ( Cohesion (..),
    Erased (..),
    Hiding (Hidden, Instance, NotHidden),
    Induction (..),
    LensCohesion (getCohesion),
    LensHiding (getHiding),
    LensQuantity (getQuantity),
    LensRelevance (getRelevance),
    Lock (..),

    LockOrigin (..),

    MetaId (MetaId),
    NameId (..),
    Named (namedThing),

    OverlapMode (..),

    Quantity (..),
    QωOrigin (..),
    Relevance (..),
    RewriteEqn' (..),
    asQuantity,
    OriginRelevant (..), 
    OriginIrrelevant (..), 
    OriginShapeIrrelevant (..), 
    PolarityModality (..),
    ModalPolarity (..),
  )
import Agda.Utils.Functor ((<&>))
import Agda.Utils.List1 (toList)
import qualified Agda.Utils.List1 as List1
import qualified Agda.Utils.Null as Agda
import Render.Class
import Render.RichText
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | NameId
instance Render NameId where
  render (NameId n m) = text $ show n ++ "@" ++ show m

-- | MetaId
instance Render MetaId where
  render (MetaId n m) = text $ "_" ++ show n ++ "@" ++ show m

-- | OriginRelevant
instance Render OriginRelevant where
  render = \case
    ORelInferred {} -> mempty
    ORelRelevant {} -> "@relevant"

instance Render OriginIrrelevant where
  render = \case
    OIrrInferred {} -> mempty
    OIrrDot {} -> "."
    OIrrIrr {} -> "@irr"
    OIrrIrrelevant {} -> "@irrelevant"

instance Render OriginShapeIrrelevant where
  render = \case
    OShIrrInferred {} -> mempty
    OShIrrDotDot {} -> ".."
    OShIrrShIrr {} -> "@shirr"
    OShIrrShapeIrrelevant {} -> "@shape-irrelevant"

-- | Relevance
#if MIN_VERSION_Agda(2,8,0)
instance Render Relevance where
  render (Relevant o) = render o
  render (Irrelevant o) = Agda.ifNull (render o) "." id
  render (ShapeIrrelevant o) = Agda.ifNull (render o) ".." id
#else
instance Render Relevance where
  render Relevant = mempty
  render Irrelevant = "."
  render NonStrict = ".."
#endif

-- | Quantity
instance Render Quantity where
  render = \case
    Quantity0 o ->
      let s = show o
       in if Agda.null o
            then "@0"
            else text s
    Quantity1 o ->
      let s = show o
       in if Agda.null o
            then "@1"
            else text s
    Quantityω o -> render o

instance Render QωOrigin where
  render = \case
    QωInferred -> mempty
    Qω {} -> "@ω"
    QωPlenty {} -> "@plenty"

instance Render Cohesion where
  render Flat = "@♭"
  render Continuous = mempty
  render Squash = "@⊤"

-- | Polarity

instance Render ModalPolarity where
  render p = case p of
    UnusedPolarity -> "@unused"
    StrictlyPositive -> "@++"
    Positive -> "@+"
    Negative -> "@-"
    MixedPolarity -> mempty

instance Render PolarityModality where
  render (PolarityModality p _ _) = render p

--------------------------------------------------------------------------------

#if MIN_VERSION_Agda(2,7,0)
instance Render OverlapMode where
  render = \case
    Overlappable -> "OVERLAPPABLE"
    Overlapping -> "OVERLAPPING"
    Incoherent -> "INCOHERENT"
    Overlaps -> "OVERLAPS"
    FieldOverlap -> "overlap"
    DefaultOverlap -> mempty
#endif

--------------------------------------------------------------------------------

-- | From 'prettyHiding'
--   @renderHiding info visible text@ puts the correct braces
--   around @text@ according to info @info@ and returns
--   @visible text@ if the we deal with a visible thing.
renderHiding :: (LensHiding a) => a -> (Inlines -> Inlines) -> Inlines -> Inlines
renderHiding a parensF =
  case getHiding a of
    Hidden -> braces'
    Instance {} -> dbraces
    NotHidden -> parensF

renderRelevance :: (LensRelevance a) => a -> Inlines -> Inlines
renderRelevance a d =
  if show d == "_" then d else render (getRelevance a) <> d

renderQuantity :: (LensQuantity a) => a -> Inlines -> Inlines
renderQuantity a d =
  if show d == "_" then d else render (getQuantity a) <+> d

instance Render Lock where
  render = \case
#if MIN_VERSION_Agda(2,6,4)
    IsLock LockOLock -> "@lock"
    IsLock LockOTick -> "@tick"
#else
    IsLock -> "@lock"
#endif
    IsNotLock -> mempty

#if MIN_VERSION_Agda(2,7,0)
renderErased :: Erased -> Inlines -> Inlines
renderErased = renderQuantity . asQuantity
#endif

renderCohesion :: (LensCohesion a) => a -> Inlines -> Inlines
renderCohesion a d =
  if show d == "_" then d else render (getCohesion a) <+> d

--------------------------------------------------------------------------------

instance (Render p, Render e) => Render (RewriteEqn' qn nm p e) where
  render = \case
    Rewrite es -> prefixedThings (text "rewrite") (render . snd <$> toList es)
    Invert _ pes -> prefixedThings (text "invert") (toList pes <&> (\(p, e) -> render p <+> "<-" <+> render e) . namedThing)
#if MIN_VERSION_Agda(2,7,0)
    LeftLet pes  -> prefixedThings (text "using") [render p <+> "<-" <+> render e | (p, e) <- List1.toList pes]
#endif

prefixedThings :: Inlines -> [Inlines] -> Inlines
prefixedThings kw = \case
  [] -> mempty
  (doc : docs) -> fsep $ (kw <+> doc) : fmap ("|" <+>) docs

instance Render Induction where
  render Inductive = "inductive"
  render CoInductive = "coinductive"
