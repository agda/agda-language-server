{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Render.Position where

import Agda.Syntax.Position
import Agda.Utils.FileName
import qualified Data.Strict.Maybe as Strict
import Render.Class
import Render.RichText

instance Render AbsolutePath where
  render = text . filePath

instance Render RangeFile where
  render = render . rangeFilePath -- TODO rangeFileName ?

--------------------------------------------------------------------------------

instance (Render a) => Render (Position' (Strict.Maybe a)) where
  render (Pn Strict.Nothing _ l c) = render l <> "," <> render c
  render (Pn (Strict.Just f) _ l c) =
    render f <> ":" <> render l <> "," <> render c

instance Render PositionWithoutFile where
  render (Pn () _ l c) = render l <> "," <> render c

instance Render IntervalWithoutFile where
#if MIN_VERSION_Agda(2,8,0)
  render (Interval () s e) = start <> "-" <> end
#else
  render (Interval s e) = start <> "-" <> end
#endif
    where
      sl = posLine s
      el = posLine e
      sc = posCol s
      ec = posCol e

      start = render sl <> "," <> render sc

      end
        | sl == el = render ec
        | otherwise = render el <> "," <> render ec

instance (Render a) => Render (Interval' (Strict.Maybe a)) where
#if MIN_VERSION_Agda(2,8,0)
  render i@(Interval f s e) = file <> render (Interval () s e)
    where
      file :: Inlines
      file = case f of
               Strict.Nothing -> mempty
               Strict.Just f  -> render f <> ":"
#else
  render i@(Interval s _) = file <> render (setIntervalFile () i)
    where
      file :: Inlines
      file = case srcFile s of
        Strict.Nothing -> mempty
        Strict.Just f -> render f <> ":"
#endif

instance (Render a) => Render (Range' (Strict.Maybe a)) where
  render r = maybe mempty render (rangeToIntervalWithFile r)
