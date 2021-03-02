{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Render
  ( RichText (..),
    LinkTarget (..),
    Render (..),
    RenderTCM (..),
    renderA,
    renderP,
    renderATop,
    space,
    string,
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
import qualified Agda.TypeChecking.Monad.Base as Agda
import qualified Agda.Utils.FileName as Agda
import qualified Agda.Syntax.Translation.AbstractToConcrete as Agda
import Data.Aeson
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Strict.Maybe as Strict
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Agda.Utils.Pretty (Doc)
import qualified Agda.Utils.Pretty as Doc
import qualified Data.List as List
import Agda.Syntax.Fixity (Precedence(TopCtx))

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

string :: String -> RichText
string s = RichText $ Seq.singleton $ Elem s mempty

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
    Hole Int
  | -- | Pointing to some places in a file
    SrcLoc Agda.Range
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

-- | Typeclass for rendering RichText
class Render a where
  render :: a -> RichText

-- | Rendering undersome context
class RenderTCM a where
  renderTCM :: a -> Agda.TCM RichText

-- | Simply "pure . render"
renderA :: (Applicative m, Render a) => a -> m RichText
renderA = pure . render

-- | Render instances of Pretty 
renderP :: (Applicative m, Doc.Pretty a) => a -> m RichText
renderP = pure . string . Doc.render . Doc.pretty 

-- | like 'prettyATop'
renderATop :: (RenderTCM c, Agda.ToConcrete a c) => a -> Agda.TCM RichText
renderATop x = Agda.abstractToConcreteCtx TopCtx x >>= renderTCM

-- | Other instances of Render
instance Render Int where
  render = string . show

instance Render Doc where
  render = string . Doc.render

instance Render a => Render [a] where
  render xs = "[" <> sepBy' ", " (map render xs) <> "]"

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
