module Render.Utils where

import Render.Class
import Render.RichText
import Agda.Utils.Time
import Agda.Utils.Pretty (pretty)

instance Render CPUTime where
  render = text . show . pretty
