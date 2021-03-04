{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Common where

import Agda.Syntax.Common
import qualified Agda.Utils.Null as Agda
import Render.Class
import Render.RichText
import Agda.Utils.Functor ((<&>))

--------------------------------------------------------------------------------

-- | NameId
instance Render NameId where
  render (NameId n m) = text $ show n ++ "@" ++ show m

-- | MetaId
instance Render MetaId where
  render (MetaId n) = text $ "_" ++ show n

-- | Relevance
instance Render Relevance where
  render Relevant = mempty
  render Irrelevant = "."
  render NonStrict = ".."

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
    Qω{}       -> "@ω"
    QωPlenty{} -> "@plenty"

instance Render Cohesion where
  render Flat   = "@♭"
  render Continuous = mempty
  render Squash  = "@⊤"

--------------------------------------------------------------------------------

-- | From 'prettyHiding'
--   @renderHiding info visible text@ puts the correct braces
--   around @text@ according to info @info@ and returns
--   @visible text@ if the we deal with a visible thing.
renderHiding :: LensHiding a => a -> (RichText -> RichText) -> RichText -> RichText
renderHiding a parensF =
  case getHiding a of
    Hidden -> braces'
    Instance {} -> dbraces
    NotHidden -> parensF

renderRelevance :: LensRelevance a => a -> RichText -> RichText
renderRelevance a d =
  if show d == "_" then d else render (getRelevance a) <> d

renderQuantity :: LensQuantity a => a -> RichText -> RichText
renderQuantity a d =
  if show d == "_" then d else render (getQuantity a) <+> d

renderCohesion :: LensCohesion a => a -> RichText -> RichText
renderCohesion a d =
  if show d == "_" then d else render (getCohesion a) <+> d

--------------------------------------------------------------------------------


instance (Render p, Render e) => Render (RewriteEqn' qn p e) where
  render = \case
    Rewrite es   -> prefixedThings (text "rewrite") (render . snd <$> es)
    Invert _ pes -> prefixedThings (text "invert") (pes <&> \ (p, e) -> render p <+> "<-" <+> render e)

prefixedThings :: RichText -> [RichText] -> RichText
prefixedThings kw = \case
  [] -> mempty
  (doc : docs) -> fsep $ (kw <+> doc) : map ("|" <+>) docs
