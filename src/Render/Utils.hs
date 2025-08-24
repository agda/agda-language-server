{-# LANGUAGE CPP #-}

module Render.Utils where

import Agda.Utils.Time (CPUTime)
import Agda.Syntax.Common.Pretty (pretty)
import Render.Class
import Render.RichText

instance Render CPUTime where
  render = text . show . pretty
