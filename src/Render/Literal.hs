module Render.Literal where

import Agda.Syntax.Literal (Literal (..), showChar', showText)
import Render.Class
import Render.Common ()
import Render.Name ()
import Render.RichText

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