{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.RichText
  ( RichText (..),
    LinkTarget (..),
    space,
    text,
    -- link,
    linkRange,
    linkHole,
    icon,
    -- combinators
    (<+>),
    braces,
    braces',
    dbraces,
    parens,
    mparens,
    parensM,
    indent,
    indentM,
    vsepM,
    sepBy,
    sepByM,
    sep,
    fsep,
    hsep,
    vcat,
    fcat,
    -- symbols
    arrow,
    lambda,
    forallQ
  )
where

import qualified Agda.Interaction.Options.IORefs as Agda
import qualified Agda.Syntax.Common as Agda
import qualified Agda.Syntax.Position as Agda
import qualified Agda.Utils.FileName as Agda
import qualified Agda.Utils.Null as Agda
import Agda.Utils.Pretty (Doc)
import qualified Agda.Utils.Pretty as Doc
import Data.Aeson (ToJSON (toJSON), Value (Null))
import Data.Foldable (toList)
import Data.IORef (readIORef)
import qualified Data.List as List
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Strict.Maybe as Strict
import Data.String (IsString (..))
import GHC.Generics (Generic)
import qualified GHC.IO.Unsafe as UNSAFE

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

(<+>) :: RichText -> RichText -> RichText
x <+> y = x <+> y

-- | Whitespace
space :: RichText
space = " "

text :: String -> RichText
text s = RichText $ Seq.singleton $ Elem s mempty

-- link :: LinkTarget -> RichText -> RichText
-- link l (RichText xs) = RichText $ fmap (overwriteLink l) xs

linkRange :: Agda.Range -> RichText -> RichText
linkRange l (RichText xs) = RichText $ fmap (overwriteLink (LinkRange l)) xs

linkHole :: Int -> RichText -> RichText
linkHole i (RichText xs) = RichText $ fmap (addClassName ["component-hole"] . overwriteLink (LinkHole i)) xs

icon :: String -> RichText
icon s = RichText $ Seq.singleton $ Elem mempty (mempty {attrIcon = Just s})

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
  { attrClassNames :: [String],
    attrLink :: Maybe LinkTarget,
    attrIcon :: Maybe String
  }
  deriving (Generic, Eq, Show)

instance ToJSON Attributes

-- | Merging Attributes with (<>)
instance Semigroup Attributes where
  Attributes a1 b1 c1 <> Attributes a2 b2 c2 = Attributes (a1 <> a2) (b1 <+> b2) (c1 <+> c2)
    where
      (<+>) :: Maybe a -> Maybe a -> Maybe a
      (<+>) (Just x) (Just _) = Just x
      (<+>) Nothing x = x
      (<+>) x Nothing = x

instance Monoid Attributes where
  mempty = Attributes [] Nothing Nothing

overwriteLink :: LinkTarget -> Element -> Element
overwriteLink target (Elem s attr) = Elem s (attr {attrLink = Just target})

addClassName :: [String] -> Element -> Element
addClassName classNames (Elem s attr) = Elem s (attr {attrClassNames = attrClassNames attr ++ classNames})

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

--------------------------------------------------------------------------------

-- | Utilities / Combinators

-- TODO: change how Element works
parens :: RichText -> RichText
parens x = "(" <> x <> ")"

parensM :: (Semigroup (f RichText), Applicative f) => f RichText -> f RichText
parensM x = pure "(" <> x <> pure ")"

-- TODO: implement this
indent :: RichText -> RichText
indent x = "  " <> x

indentM :: (Semigroup (f RichText), Applicative f) => f RichText -> f RichText
indentM x = pure "  " <> x

sepBy :: RichText -> [RichText] -> RichText
sepBy delim [] = mempty
sepBy delim (x : xs) = x <> delim <> sepBy delim xs

sepByM :: Applicative f => RichText -> [RichText] -> f RichText
sepByM d = pure . sepBy d

-- TODO: implement this
vsepM :: Applicative f => [RichText] -> f RichText
vsepM = sepByM " "

sep :: [RichText] -> RichText
sep = sepBy " "

fsep :: [RichText] -> RichText
fsep = sepBy " "

hsep :: [RichText] -> RichText
hsep = sepBy " "

vcat :: [RichText] -> RichText
vcat = sepBy " "

fcat :: [RichText] -> RichText
fcat = sepBy " "

-- | Single braces
braces :: RichText -> RichText
braces x = "{" <> x <> "}"

-- | Double braces
dbraces :: RichText -> RichText
dbraces = _dbraces specialCharacters

arrow :: RichText
arrow = _arrow specialCharacters

lambda :: RichText
lambda = _lambda specialCharacters

forallQ :: RichText
forallQ = _forallQ specialCharacters

-- | Apply 'parens' to 'Doc' if boolean is true.
mparens :: Bool -> RichText -> RichText
mparens True = parens
mparens False = id

-- | From braces'
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

--------------------------------------------------------------------------------

-- | Picking the appropriate set of special characters depending on
-- whether we are allowed to use unicode or have to limit ourselves
-- to ascii.
data SpecialCharacters = SpecialCharacters
  { _dbraces :: RichText -> RichText,
    _lambda :: RichText,
    _arrow :: RichText,
    _forallQ :: RichText,
    _leftIdiomBrkt :: RichText,
    _rightIdiomBrkt :: RichText,
    _emptyIdiomBrkt :: RichText
  }

{-# NOINLINE specialCharacters #-}
specialCharacters :: SpecialCharacters
specialCharacters =
  let opt = UNSAFE.unsafePerformIO (readIORef Agda.unicodeOrAscii)
   in case opt of
        Agda.UnicodeOk ->
          SpecialCharacters
            { _dbraces = ("\x2983 " <>) . (<> " \x2984"),
              _lambda = "\x03bb",
              _arrow = "\x2192",
              _forallQ = "\x2200",
              _leftIdiomBrkt = "\x2987",
              _rightIdiomBrkt = "\x2988",
              _emptyIdiomBrkt = "\x2987\x2988"
            }
        Agda.AsciiOnly ->
          SpecialCharacters
            { _dbraces = braces . braces',
              _lambda = "\\",
              _arrow = "->",
              _forallQ = "forall",
              _leftIdiomBrkt = "(|",
              _rightIdiomBrkt = "|)",
              _emptyIdiomBrkt = "(|)"
            }
