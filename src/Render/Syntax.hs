{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Render.Syntax where

import Agda.Syntax.Common
-- import Agda.Syntax.Literal
import Agda.Syntax.Concrete
import Agda.TypeChecking.Monad.Base
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import qualified Agda.Utils.Pretty as Agda
import Render.Class
import Render.Literal ()
import Render.Name ()
import Render.RichText
import Agda.Syntax.Position (noRange)

--------------------------------------------------------------------------------

-- | Expression
instance Render Expr where
  render expr = case expr of
    Ident qname -> render qname
    Lit lit -> render lit
    -- no hole index, use LinkRange instead
    QuestionMark range Nothing -> linkRange range "?"
    QuestionMark _range (Just n) -> linkHole n $ "?" <> text (show n)
    Underscore range n -> linkRange range $ maybe "_" text n
    App {} ->
      case appView expr of
        AppView e1 args ->
          sepBy " " $ render e1 : map render args
    RawApp _ es -> sepBy " " $ map render es
    OpApp _ q _ es -> sepBy " " $ prettyOpApp q es
    WithApp _ e es -> sepBy " | " $ map render (e:es)
    HiddenArg _ e -> braces' $ render e
    others -> text $ show (Agda.pretty others)

prettyOpApp ::
  forall a.
  Render a =>
  QName ->
  [NamedArg (MaybePlaceholder a)] ->
  [RichText]
prettyOpApp q es = merge [] $ prOp ms xs es
  where
    -- ms: the module part of the name.
    ms = init (qnameParts q)
    -- xs: the concrete name (alternation of @Id@ and @Hole@)
    xs = case unqualify q of
      Name _ _ xs -> xs
      NoName {} -> __IMPOSSIBLE__

    prOp :: Render a => [Name] -> [NamePart] -> [NamedArg (MaybePlaceholder a)] -> [(RichText, Maybe PositionInName)]
    prOp ms (Hole : xs) (e : es) =
      case namedArg e of
        Placeholder p -> (qual ms $ render e, Just p) : prOp [] xs es
        NoPlaceholder {} -> (render e, Nothing) : prOp ms xs es
    -- Module qualifier needs to go on section holes (#3072)
    prOp _ (Hole : _) [] = __IMPOSSIBLE__
    prOp ms (Id x : xs) es =
      ( qual ms $ render $ Name noRange InScope [Id x],
        Nothing
      ) :
      prOp [] xs es
    -- Qualify the name part with the module.
    -- We then clear @ms@ such that the following name parts will not be qualified.

    prOp _ [] es = map (\e -> (render e, Nothing)) es

    qual ms doc = sepBy "." $ map render ms ++ [doc]

    -- Section underscores should be printed without surrounding
    -- whitespace. This function takes care of that.
    merge :: [RichText] -> [(RichText, Maybe PositionInName)] -> [RichText]
    merge before [] = reverse before
    merge before ((d, Nothing) : after) = merge (d : before) after
    merge before ((d, Just Beginning) : after) = mergeRight before d after
    merge before ((d, Just End) : after) = case mergeLeft d before of
      (d, bs) -> merge (d : bs) after
    merge before ((d, Just Middle) : after) = case mergeLeft d before of
      (d, bs) -> mergeRight bs d after

    mergeRight before d after =
      reverse before
        ++ case merge [] after of
          [] -> [d]
          a : as -> (d <> a) : as

    mergeLeft d before = case before of
      [] -> (d, [])
      b : bs -> (b <> d, bs)

instance RenderTCM Expr where
  renderTCM = return . render

--------------------------------------------------------------------------------

-- | OpApp
instance Render (OpApp Expr) where
  render (Ordinary e) = render e
  render (SyntaxBindingLambda r bs e) = render (Lam r bs e)

-- | MaybePlaceholder
instance Render a => Render (MaybePlaceholder a) where
  render Placeholder{}       = "_"
  render (NoPlaceholder _ e) = render e

--------------------------------------------------------------------------------

-- | InteractionId
instance Render InteractionId where
  render (InteractionId i) = linkHole i ("?" <> render i)

--------------------------------------------------------------------------------

-- | Named NamedName (Named_)
instance Render e => Render (Named NamedName e) where
  renderPrec p (Named nm e)
    | Just s <- bareNameOf nm = mparens (p > 0) $ sepBy " " [text s <> " =", render e]
    | otherwise = renderPrec p e
