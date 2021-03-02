{-# LANGUAGE OverloadedStrings #-}

module Render.Name where

import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Concrete as C
import Render
import Agda.Syntax.Common ( rawNameToString )

instance Render C.NamePart where
  render C.Hole = "_"
  render (C.Id s) = string $ rawNameToString s

instance Render C.Name where
  render (C.Name _ _ xs) = sepBy' "." (map render xs)
  render (C.NoName _ _) = "_"

instance Render A.Name where
  render = render . A.nameConcrete

instance Render A.QName where
  render (A.QName m x) = sepBy' "." (map render (A.mnameToList m ++ [x]))

