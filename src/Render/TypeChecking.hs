{-# LANGUAGE OverloadedStrings #-}

module Render.TypeChecking where

import Agda.Syntax.Common
import Agda.TypeChecking.Monad.Base
import Render.Class
import Render.RichText

instance Render NamedMeta where
  render (NamedMeta "" (MetaId x)) = render x
  render (NamedMeta "_" (MetaId x)) = render x
  render (NamedMeta s (MetaId x)) = "_" <> text s <> render x
