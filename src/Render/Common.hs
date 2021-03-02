{-# LANGUAGE OverloadedStrings #-}

module Render.Common where

import Agda.Syntax.Common
import Render.Class
import Render.RichText

--------------------------------------------------------------------------------

-- | NameId
instance Render NameId where
  render (NameId n m) = text $ show n ++ "@" ++ show m

-- | MetaId
instance Render MetaId where
  render (MetaId n) = text $ "_" ++ show n
