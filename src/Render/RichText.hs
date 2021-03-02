{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Render.RichText
  ( RichText (..),
    LinkTarget (..),
    space,
    text,
    link,
    icon,
    parens,
    indent,
    vsep,
    sepBy',
    sepBy
  )
where

import qualified Agda.Syntax.Position as Agda
import qualified Agda.Utils.FileName as Agda
import Data.Aeson ( Value(Null), ToJSON(toJSON) )
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Strict.Maybe as Strict
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Agda.Utils.Pretty (Doc)
import qualified Agda.Utils.Pretty as Doc
import qualified Data.List as List

--------------------------------------------------------------------------------

newtype RichText = RichText (Seq Element) deriving (Show)

merge :: Seq Element -> Seq Element -> Seq Element
merge xs Seq.Empty = xs
merge xs (y :<| ys) = merge (mergeOne xs y) ys

mergeOne :: Seq Element -> Element -> Seq Element
mergeOne Empty (Elem y b) = Seq.singleton (Elem y b)
mergeOne (xs :|> Elem x a) (Elem y b) =
  if a == b then xs :|> Elem (x <> y) a else xs :|> Elem x a :|> Elem y b

-- Represent RichText with String literals
instance IsString RichText where
  fromString s = RichText (Seq.singleton (Elem s mempty))

instance Semigroup RichText where
  RichText xs <> RichText ys = RichText (merge xs ys)

instance Monoid RichText where
  mempty = RichText mempty

instance ToJSON RichText where
  toJSON (RichText xs) = toJSON xs

-- toJSON (merge Empty xs)

-- | Whitespace
space :: RichText
space = " "

text :: String -> RichText
text s = RichText $ Seq.singleton $ Elem s mempty

link :: LinkTarget -> RichText -> RichText
link l (RichText xs) = RichText $ fmap (overwriteLink l) xs

icon :: String -> RichText
icon s = RichText $ Seq.singleton $ Elem mempty (mempty {attrIcon = Just s})

-- TODO: change how Element works
parens :: (Semigroup (f RichText), Applicative f) => f RichText -> f RichText
parens x = pure "(" <> x <> pure ")"

-- TODO: implement this
indent :: (Semigroup (f RichText), Applicative f) => f RichText -> f RichText
indent x = pure " " <> x

sepBy' :: RichText -> [RichText] -> RichText
sepBy' delim [] = mempty 
sepBy' delim (x:xs) = x <> delim <> sepBy' delim xs

sepBy :: Applicative f => RichText -> [RichText] -> f RichText
sepBy d = pure . sepBy' d 

-- TODO: implement this
vsep :: Applicative f => [RichText] -> f RichText
vsep = sepBy " "

--------------------------------------------------------------------------------

data LinkTarget
  = -- | Pointing to some hole
    LinkHole Int
  | -- | Pointing to some places in a file
    LinkRange Agda.Range
  deriving (Generic, Eq, Show)

instance ToJSON LinkTarget

--------------------------------------------------------------------------------

data Attributes = Attributes
  { attrLink :: Maybe LinkTarget,
    attrIcon :: Maybe String
  }
  deriving (Generic, Eq, Show)

instance ToJSON Attributes

-- | Merging Attributes with (<>)
instance Semigroup Attributes where
  Attributes a1 b1 <> Attributes a2 b2 = Attributes (a1 <+> a2) (b1 <+> b2)
    where
      (<+>) :: Maybe a -> Maybe a -> Maybe a
      (<+>) (Just x) (Just _) = Just x
      (<+>) Nothing x = x
      (<+>) x Nothing = x

instance Monoid Attributes where
  mempty = Attributes Nothing Nothing

overwriteLink :: LinkTarget -> Element -> Element
overwriteLink target (Elem s attr) = Elem s (attr {attrLink = Just target})

--------------------------------------------------------------------------------

-- | Internal type, to be converted to JSON values
data Element = Elem String Attributes
  deriving (Generic, Show)

instance ToJSON Element

--------------------------------------------------------------------------------

-- | ToJSON instances for Agda types
instance ToJSON Agda.Range

instance ToJSON (Agda.Interval' ()) where
  toJSON (Agda.Interval start end) = toJSON (start, end)

instance ToJSON (Agda.Position' ()) where
  toJSON (Agda.Pn () pos line col) = toJSON [line, col, pos]

instance ToJSON Agda.SrcFile where
  toJSON Strict.Nothing = Null
  toJSON (Strict.Just path) = toJSON path

instance ToJSON Agda.AbsolutePath where
  toJSON (Agda.AbsolutePath path) = toJSON path
