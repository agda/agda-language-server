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

-- glueing name parts together 
instance Render C.Name where
  render (C.Name range _inScope xs) = linkRange range $ mconcat (map render xs)
  render (C.NoName _ _) = "_"

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
  render = hcat . punctuate "." . map render . A.qnameToList
