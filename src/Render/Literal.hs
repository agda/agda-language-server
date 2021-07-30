{-# LANGUAGE OverloadedStrings #-}

module Render.Literal where

import Data.Text (Text)
import qualified Data.Text as T
import Agda.Syntax.Literal
import Render.Class
import Render.RichText
import Render.Name ()
import Render.Common ()

--------------------------------------------------------------------------------

-- | Literal
instance Render Literal where
  render (LitNat n) = text $ show n
  render (LitWord64 n) = text $ show n
  render (LitFloat d) = text $ show d
  render (LitString s) = text $ showText s ""
  render (LitChar c) = text $ "'" ++ showChar' c "'"
  render (LitQName x) = render x
  render (LitMeta _ x) = render x