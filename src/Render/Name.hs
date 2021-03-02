{-# LANGUAGE OverloadedStrings #-}

module Render.Name where

import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Concrete as C
import Render.Class
import Render.RichText
import qualified Agda.Syntax.Common as C


--------------------------------------------------------------------------------

-- | Concrete 
instance Render C.NamePart where
  render C.Hole = "_"
  render (C.Id s) = text $ C.rawNameToString s

instance Render C.Name where
  render (C.Name range _inScope xs) = linkRange range $ sepBy " " (map render xs)
  render (C.NoName range _) = "_"

instance Render C.QName where
  render (C.Qual m x)
    | C.isUnderscore m = render x -- don't print anonymous modules
    | otherwise      = render m <> "." <> render x
  render (C.QName x)  = render x


--------------------------------------------------------------------------------

-- | Abstract 
instance Render A.Name where
  render = render . A.nameConcrete

instance Render A.QName where
  render (A.QName m x) = sepBy "." (map render (A.mnameToList m ++ [x]))
