{-# LANGUAGE CPP #-}

module Render.Utils where

import Agda.Utils.Time ( CPUTime )
#if MIN_VERSION_Agda(2,6,4)
import Agda.Syntax.Common.Pretty (pretty)
#else
import Agda.Utils.Pretty (pretty)
#endif

import Render.Class
import Render.RichText

instance Render CPUTime where
  render = text . show . pretty
