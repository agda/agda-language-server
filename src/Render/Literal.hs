{-# LANGUAGE OverloadedStrings #-}

module Render.Literal where

import Agda.Syntax.Common
import Agda.Syntax.Literal
import Agda.TypeChecking.Monad.Base
import qualified Agda.Utils.Pretty as Agda
import Render.Class
import Render.RichText
import Render.Name ()
import Render.Common ()

--------------------------------------------------------------------------------

-- | Literal
instance Render Literal where
  render (LitNat _ n) = text $ show n
  render (LitWord64 _ n) = text $ show n
  render (LitFloat _ d) = text $ show d
  render (LitString _ s) = text $ showString' s ""
  render (LitChar _ c) = text $ "'" ++ showChar' c "'"
  render (LitQName _ x) = render x
  render (LitMeta _ _ x) = render x