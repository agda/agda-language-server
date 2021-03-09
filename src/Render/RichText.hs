{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.RichText
  ( Inlines (..),
    -- LinkTarget (..),
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
    vsep,
    vsepM,
    hcat,
    hsep,
    sepBy,
    sepByM,
    sep,
    fsep,
    vcat,
    fcat,
    -- symbols
    arrow,
    lambda,
    forallQ,
    showIndex,
    leftIdiomBrkt,
    rightIdiomBrkt,
    emptyIdiomBrkt,
  )
where

import qualified Agda.Interaction.Options.IORefs as Agda
import qualified Agda.Syntax.Position as Agda
import qualified Agda.Utils.FileName as Agda
import qualified Agda.Utils.Null as Agda
import Agda.Utils.Suffix (subscriptAllowed, toSubscriptDigit)
import Data.Aeson (ToJSON (toJSON), Value (Null))
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Strict.Maybe as Strict
import Data.String (IsString (..))
import GHC.Generics (Generic)
import qualified GHC.IO.Unsafe as UNSAFE

--------------------------------------------------------------------------------

newtype Inlines = Inlines (Seq Inline)

-- Represent Inlines with String literals
instance IsString Inlines where
  fromString s = Inlines (Seq.singleton (Text s mempty))

instance Semigroup Inlines where
  Inlines xs <> Inlines ys = Inlines (xs <> ys)

instance Monoid Inlines where
  mempty = Inlines mempty

instance ToJSON Inlines where
  toJSON (Inlines xs) = toJSON xs

instance Show Inlines where
  show (Inlines xs) = unwords $ map show $ toList xs

-- | see if the rendered text is "empty"
isEmpty :: Inlines -> Bool
isEmpty (Inlines xs) = all elemIsEmpty (Seq.viewl xs)
  where
    elemIsEmpty :: Inline -> Bool
    elemIsEmpty (Text "" _) = True
    elemIsEmpty (Link _ s _) = all elemIsEmpty s
    elemIsEmpty _ = False

(<+>) :: Inlines -> Inlines -> Inlines
x <+> y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> " " <> y

-- | Whitespace
space :: Inlines
space = " "

text :: String -> Inlines
text s = Inlines $ Seq.singleton $ Text s mempty

icon :: String -> Inlines
icon s = Inlines $ Seq.singleton $ Icon s []

linkRange :: Agda.Range -> Inlines -> Inlines
linkRange range (Inlines xs) = Inlines $ Seq.singleton $ Link range (toList xs) mempty

linkHole :: Int -> Inlines
linkHole i = Inlines $ Seq.singleton $ Hole i

--------------------------------------------------------------------------------

type ClassNames = [String]

--------------------------------------------------------------------------------

-- | Internal type, to be converted to JSON values
data Inline
  = Icon String ClassNames
  | Text String ClassNames
  | Link Agda.Range [Inline] ClassNames
  | Hole Int
  | -- | Elements inside would wrap (to the next line) when there's no space
    Wrap
      Bool
      [Inline]
  deriving (Generic)

instance ToJSON Inline

instance Show Inline where
  show (Icon s _) = s
  show (Text s _) = s
  show (Link _ xs _) = mconcat (fmap show xs)
  show (Hole i) = "?" ++ show i
  show (Wrap True xs) = unwords (fmap show xs)
  show (Wrap False xs) = mconcat (fmap show xs)

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
parens :: Inlines -> Inlines
parens x = "(" <> x <> ")"

parensM :: (Semigroup (f Inlines), Applicative f) => f Inlines -> f Inlines
parensM x = pure "(" <> x <> pure ")"

-- TODO: implement this
indent :: Inlines -> Inlines
indent x = "  " <> x

indentM :: (Semigroup (f Inlines), Applicative f) => f Inlines -> f Inlines
indentM x = pure "  " <> x

sepBy :: Inlines -> [Inlines] -> Inlines
sepBy _ [] = mempty
sepBy _ [x] = x
sepBy delim (x : xs) = x <> delim <> sepBy delim xs

sepByM :: Applicative f => Inlines -> [Inlines] -> f Inlines
sepByM d = pure . sepBy d

hcat :: [Inlines] -> Inlines
hcat = mconcat

hsep :: [Inlines] -> Inlines
hsep = sepBy " "

vcat :: [Inlines] -> Inlines
vcat = sepBy " "

-- TODO: implement this
vsep :: [Inlines] -> Inlines
vsep = sepBy " "

vsepM :: Applicative f => [Inlines] -> f Inlines
vsepM = sepByM " "

sep :: [Inlines] -> Inlines
sep = sepBy " "

fsep :: [Inlines] -> Inlines
fsep = sepBy " "

fcat :: [Inlines] -> Inlines
fcat = sepBy " "

-- | Single braces
braces :: Inlines -> Inlines
braces x = "{" <> x <> "}"

-- | Double braces
dbraces :: Inlines -> Inlines
dbraces = _dbraces specialCharacters

arrow :: Inlines
arrow = _arrow specialCharacters

lambda :: Inlines
lambda = _lambda specialCharacters

forallQ :: Inlines
forallQ = _forallQ specialCharacters

-- left, right, and empty idiom bracket
leftIdiomBrkt, rightIdiomBrkt, emptyIdiomBrkt :: Inlines
leftIdiomBrkt = _leftIdiomBrkt specialCharacters
rightIdiomBrkt = _rightIdiomBrkt specialCharacters
emptyIdiomBrkt = _emptyIdiomBrkt specialCharacters

-- | Apply 'parens' to 'Doc' if boolean is true.
mparens :: Bool -> Inlines -> Inlines
mparens True = parens
mparens False = id

-- | From braces'
braces' :: Inlines -> Inlines
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

-- | Shows a non-negative integer using the characters ₀-₉ instead of
-- 0-9 unless the user explicitly asked us to not use any unicode characters.
showIndex :: (Show i, Integral i) => i -> String
showIndex = case subscriptAllowed of
  Agda.UnicodeOk -> map toSubscriptDigit . show
  Agda.AsciiOnly -> show

--------------------------------------------------------------------------------

-- | Picking the appropriate set of special characters depending on
-- whether we are allowed to use unicode or have to limit ourselves
-- to ascii.
data SpecialCharacters = SpecialCharacters
  { _dbraces :: Inlines -> Inlines,
    _lambda :: Inlines,
    _arrow :: Inlines,
    _forallQ :: Inlines,
    _leftIdiomBrkt :: Inlines,
    _rightIdiomBrkt :: Inlines,
    _emptyIdiomBrkt :: Inlines
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
