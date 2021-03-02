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

-- | Arg
instance Render a => Render (Arg a) where
  renderPrec p (Arg ai e) = renderHiding ai localParens $ renderPrec p' e
      where p' | visible ai = p
               | otherwise  = 0
            localParens | getOrigin ai == Substitution = parens
                        | otherwise = id
