{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Render.Class
  ( Render (..),
    RenderTCM (..),
    renderA,
    renderP,
    renderATop,
  )
where

import qualified Agda.TypeChecking.Monad.Base as Agda
import qualified Agda.Syntax.Translation.AbstractToConcrete as Agda
import Agda.Utils.Pretty (Doc)
import qualified Agda.Utils.Pretty as Doc
import Agda.Syntax.Fixity (Precedence(TopCtx))
import Render.RichText

--------------------------------------------------------------------------------

-- | Typeclass for rendering RichText
class Render a where
  render :: a -> RichText
  renderPrec  :: Int -> a -> RichText

  render = renderPrec 0
  renderPrec = const render

-- | Rendering undersome context
class RenderTCM a where
  renderTCM :: a -> Agda.TCM RichText

-- | Simply "pure . render"
renderA :: (Applicative m, Render a) => a -> m RichText
renderA = pure . render

-- | Render instances of Pretty 
renderP :: (Applicative m, Doc.Pretty a) => a -> m RichText
renderP = pure . text . Doc.render . Doc.pretty 

-- | like 'prettyATop'
renderATop :: (RenderTCM c, Agda.ToConcrete a c) => a -> Agda.TCM RichText
renderATop x = Agda.abstractToConcreteCtx TopCtx x >>= renderTCM


--------------------------------------------------------------------------------

-- | Other instances of Render
instance Render Int where
  render = text . show

instance Render Doc where
  render = text . Doc.render

instance Render a => Render [a] where
  render xs = "[" <> sepBy ", " (map render xs) <> "]"
