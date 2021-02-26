{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module RichText (RichText (..), LinkTarget (..), Render (..), RenderTCM (..), space, string, link, icon) where

import qualified Agda.Syntax.Position as Agda
import qualified Agda.TypeChecking.Monad.Base as Agda
import qualified Agda.Utils.FileName as Agda
import Data.Aeson
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Strict.Maybe as Strict
import Data.String (IsString (..))
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

newtype RichText = RichText (Seq Element)

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
  toJSON (RichText xs) = toJSON (merge Empty xs)

-- | Whitespace
space :: RichText
space = " "

string :: String -> RichText
string s = RichText $ Seq.singleton $ Elem s mempty

link :: LinkTarget -> RichText -> RichText
link l (RichText xs) = RichText $ fmap (overwriteLink l) xs

icon :: String -> RichText
icon s = RichText $ Seq.singleton $ Elem mempty (mempty {attrIcon = Just s})

--------------------------------------------------------------------------------

data LinkTarget
  = -- | Pointing to some hole
    Hole Int
  | -- | Pointing to some places in a file
    SrcLoc Agda.Range
  deriving (Generic, Eq)

instance ToJSON LinkTarget

--------------------------------------------------------------------------------

data Attributes = Attributes
  { attrLink :: Maybe LinkTarget,
    attrIcon :: Maybe String
  }
  deriving (Generic, Eq)

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
  deriving (Generic)

instance ToJSON Element

--------------------------------------------------------------------------------

-- | Typeclass for rendering RichText
class Render a where
  render :: a -> RichText

instance Render Int where 
  render = string . show 
  
class RenderTCM a where
  renderTCM :: a -> Agda.TCM RichText

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
