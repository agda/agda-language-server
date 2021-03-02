{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.RichText
  ( RichText (..),
    LinkTarget (..),
    space,
    text,
    link,
    icon,
    -- combinators
    parens,
    mparens,
    parensM,
    indentM,
    vsepM,
    sepBy,
    sepByM,
    -- utilities
    renderHiding,
  )
where

import qualified Agda.Syntax.Common as Agda
import qualified Agda.Syntax.Position as Agda
import qualified Agda.Utils.FileName as Agda
import qualified Agda.Utils.Null as Agda
import Agda.Utils.Pretty (Doc)
import qualified Agda.Utils.Pretty as Doc
import Data.Aeson (ToJSON (toJSON), Value (Null))
import qualified Data.List as List
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Strict.Maybe as Strict
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Data.Foldable (toList)

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
  toJSON (RichText xs) = toJSON xs

instance Show RichText where
  show (RichText xs) = unwords $ map show $ toList xs

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
parens :: RichText -> RichText
parens x = "(" <> x <> ")"

parensM :: (Semigroup (f RichText), Applicative f) => f RichText -> f RichText
parensM x = pure "(" <> x <> pure ")"

-- TODO: implement this
indentM :: (Semigroup (f RichText), Applicative f) => f RichText -> f RichText
indentM x = pure " " <> x

sepBy :: RichText -> [RichText] -> RichText
sepBy delim [] = mempty
sepBy delim (x : xs) = x <> delim <> sepBy delim xs

sepByM :: Applicative f => RichText -> [RichText] -> f RichText
sepByM d = pure . sepBy d

-- TODO: implement this
vsepM :: Applicative f => [RichText] -> f RichText
vsepM = sepByM " "

braces :: RichText -> RichText
braces x = "{" <> x <> "}"

dbraces :: RichText -> RichText
dbraces x = "{" <> x <> "}"

-- | Apply 'parens' to 'Doc' if boolean is true.
mparens :: Bool -> RichText -> RichText
mparens True  = parens
mparens False = id

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
  deriving (Generic)

instance ToJSON Element

instance Show Element where
  show (Elem s _) = s

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

-- | Utilities

-- | From 'prettyHiding'
--   @renderHiding info visible text@ puts the correct braces
--   around @text@ according to info @info@ and returns
--   @visible text@ if the we deal with a visible thing.
renderHiding :: Agda.LensHiding a => a -> (RichText -> RichText) -> RichText -> RichText
renderHiding a parens =
  case Agda.getHiding a of
    Agda.Hidden -> braces'
    Agda.Instance {} -> dbraces
    Agda.NotHidden -> parens
-- | From 'braces\''
braces' :: RichText -> RichText
braces' d =
  let s = show d
   in if Agda.null s
        then braces d
        else braces (spaceIfDash (head s) <> d <> spaceIfDash (last s))
  where
    -- Add space to avoid starting a comment (Ulf, 2010-09-13, #269)
    -- Andreas, 2018-07-21, #3161: Also avoid ending a comment
    spaceIfDash '-' = " "
    spaceIfDash _ = mempty