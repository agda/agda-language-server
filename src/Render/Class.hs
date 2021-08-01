{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Render.Class
  ( Render (..),
    -- RenderTCM (..),
    renderM,
    renderP,
    renderA,
    renderATop,
  )
where

import           Agda.Syntax.Fixity (Precedence (TopCtx))
import qualified Agda.Syntax.Translation.AbstractToConcrete as A
import qualified Agda.TypeChecking.Monad.Base               as A
import           Agda.Utils.Pretty (Doc)
import qualified Agda.Utils.Pretty as Doc

import Data.Int (Int32)
import Render.RichText

--------------------------------------------------------------------------------

-- | Typeclass for rendering Inlines
class Render a where
  render :: a -> Inlines
  renderPrec :: Int -> a -> Inlines

  render = renderPrec 0
  renderPrec = const render

-- | Rendering undersome context
-- class RenderTCM a where
--   renderTCM :: a -> Agda.TCM Inlines

-- | Simply "pure . render"
renderM :: (Applicative m, Render a) => a -> m Inlines
renderM = pure . render

-- | Render instances of Pretty
renderP :: (Applicative m, Doc.Pretty a) => a -> m Inlines
renderP = pure . text . Doc.render . Doc.pretty

-- | like 'prettyA'
renderA :: (Render c, A.ToConcrete a, A.ConOfAbs a ~ c) => a -> A.TCM Inlines
renderA x = render <$> A.abstractToConcrete_ x 

-- | like 'prettyATop'
renderATop :: (Render c, A.ToConcrete a, A.ConOfAbs a ~ c) => a -> A.TCM Inlines
renderATop x = render <$> A.abstractToConcreteCtx TopCtx x

--------------------------------------------------------------------------------

-- | Other instances of Render
instance Render Int where
  render = text . show

instance Render Int32 where
  render = text . show

instance Render Integer where
  render = text . show

instance Render Bool where
  render = text . show

instance Render Doc where
  render = text . Doc.render

instance Render a => Render [a] where
  render xs = "[" <> fsep (punctuate "," (map render xs)) <> "]"
