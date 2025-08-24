{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Render.Class
  ( Render (..),
    -- RenderTCM (..),
    renderM,
    renderP,
    renderA,
    renderATop,
  )
where

import Agda.Syntax.Fixity (Precedence (TopCtx))
import qualified Agda.Syntax.Translation.AbstractToConcrete as A
import qualified Agda.TypeChecking.Monad.Base as A
import Agda.Utils.List1 (List1)
import Agda.Utils.List2 (List2)
import           Agda.Syntax.Common.Pretty (Doc)
import qualified Agda.Syntax.Common.Pretty as Doc

import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Exts (IsList (toList))
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

instance Render Word32 where
  render = text . show

instance Render Integer where
  render = text . show

instance Render Bool where
  render = text . show

instance Render Text where
  render = text . show

instance Render Doc where
  render = text . Doc.render

instance (Render a) => Render (Maybe a) where
  renderPrec p Nothing = mempty
  renderPrec p (Just x) = renderPrec p x

instance (Render a) => Render [a] where
  render xs = "[" <> fsep (punctuate "," (fmap render xs)) <> "]"

instance (Render a) => Render (List1 a) where
  render = render . toList

instance (Render a) => Render (List2 a) where
  render = render . toList
