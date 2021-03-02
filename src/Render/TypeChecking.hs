{-# LANGUAGE OverloadedStrings #-}

module Render.TypeChecking where

import Render
import Agda.TypeChecking.Monad.Base
import Agda.Syntax.Translation.AbstractToConcrete (abstractToConcreteCtx)
import Agda.Syntax.Fixity (Precedence(TopCtx))
import Agda.Utils.Pretty (pretty)
import Agda.Syntax.Common

-- instance RenderTCM NamedMeta where
--   renderTCM namedMeta = do
--     namedMeta' <- abstractToConcreteCtx TopCtx namedMeta
--     return $ string $ show (pretty namedMeta')

instance Render NamedMeta where
  render (NamedMeta "" (MetaId x)) = render x
  render (NamedMeta "_" (MetaId x)) = render x
  render (NamedMeta s (MetaId x)) = "_" <> string s <> render x

