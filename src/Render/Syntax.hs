{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleInstances #-}
module Render.Syntax where

import Agda.Syntax.Common
-- import Agda.Syntax.Literal
import Agda.Syntax.Concrete
import Agda.TypeChecking.Monad.Base
import qualified Agda.Utils.Pretty as Agda
import Render.Class
import Render.Literal ()
import Render.Name ()
import Render.RichText

--------------------------------------------------------------------------------

-- | Expression
instance Render Expr where
  render expr = case expr of
    Ident qname -> render qname
    Lit lit -> render lit
    -- no hole index, use LinkRange instead
    QuestionMark range Nothing -> link (LinkRange range) "?"
    QuestionMark _range (Just n) -> link (LinkHole n) $ "?" <> text (show n)
    Underscore range n -> link (LinkRange range) $ maybe "_" text n
    App {} ->
      case appView expr of
        AppView e1 args ->
          sepBy " " $ render e1 : map render args
    others -> text $ show (Agda.pretty others)

instance RenderTCM Expr where
  renderTCM = return . render

--------------------------------------------------------------------------------

-- | InteractionId
instance Render InteractionId where
  render (InteractionId i) = link (LinkHole i) ("?" <> render i)

--------------------------------------------------------------------------------

-- | Named NamedName (Named_)
instance Render e => Render (Named NamedName e) where
  renderPrec p (Named nm e)
    | Just s <- bareNameOf nm = mparens (p > 0) $ sepBy " " [ text s <> " =", render e ]
    | otherwise               = renderPrec p e
