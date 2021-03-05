{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.TypeChecking where

import Agda.Syntax.Common
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Positivity.Occurrence
import Render.Class
import Render.RichText

instance Render NamedMeta where
  render (NamedMeta "" (MetaId x)) = render x
  render (NamedMeta "_" (MetaId x)) = render x
  render (NamedMeta s (MetaId x)) = "_" <> text s <> render x

instance Render Occurrence where
  render =
    text . \case
      Unused -> "_"
      Mixed -> "*"
      JustNeg -> "-"
      JustPos -> "+"
      StrictPos -> "++"
      GuardPos -> "g+"

instance Render ProblemId where
  render (ProblemId n) = render n

instance Render Comparison where
  render CmpEq = "="
  render CmpLeq = "=<"

instance Render Polarity where
  render =
    text . \case
      Covariant -> "+"
      Contravariant -> "-"
      Invariant -> "*"
      Nonvariant -> "_"