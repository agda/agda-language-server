{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.RichText
  ( Block(..),
    Inlines (..),
    -- LinkTarget (..),
    space,
    text,
    text',
    parens,
    -- link,
    linkRange,
    linkHole,
    icon,
    -- combinators
    (<+>),
    punctuate,
    braces,
    braces',
    dbraces,
    mparens,
    hcat,
    hsep,
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
-- | Block elements

data Block 
  -- for blocks like "Goal" & "Have"
  = Labeled Inlines (Maybe String) (Maybe Agda.Range) String String 
  -- for ordinary goals & context
  | Unlabeled Inlines (Maybe String) (Maybe Agda.Range)
  -- headers
  | Header String  
  deriving (Generic)

instance ToJSON Block

--------------------------------------------------------------------------------

newtype Inlines = Inlines {unInlines :: Seq Inline}

-- Represent Inlines with String literals
instance IsString Inlines where
  fromString s = Inlines (Seq.singleton (Text s mempty))

instance Semigroup Inlines where
  Inlines as <> Inlines bs = Inlines (merge as bs)
    where
      merge :: Seq Inline -> Seq Inline -> Seq Inline
      merge Empty      ys = ys
      merge (xs :|> x) ys = merge xs (cons x ys)

      cons :: Inline -> Seq Inline -> Seq Inline
      cons (Text s c) (Text t d :<| xs)
        -- merge 2 adjacent Text if they have the same classnames
        | c == d    = Text (s <> t) c :<| xs 
        | otherwise = Text s c :<| Text t d :<| xs
      cons (Text s c) (Horz [] :<| xs) = cons (Text s c) xs
      cons (Text s c) (Horz (Inlines t:ts) :<| xs)
        -- merge Text with Horz when possible
        = Horz (Inlines (cons (Text s c) t) :ts) :<| xs
      cons x xs = x :<| xs


instance Monoid Inlines where
  mempty = Inlines mempty

instance ToJSON Inlines where
  toJSON (Inlines xs) = toJSON xs

instance Show Inlines where
  show (Inlines xs) = unwords $ map show $ toList xs

-- | see if the rendered text is "empty"
isEmpty :: Inlines -> Bool
isEmpty (Inlines elems) = all elemIsEmpty (Seq.viewl elems)
  where
    elemIsEmpty :: Inline -> Bool
    elemIsEmpty (Icon _ _) = False
    elemIsEmpty (Text "" _) = True
    elemIsEmpty (Text _ _) = False
    elemIsEmpty (Link _ xs _) = all elemIsEmpty $ unInlines xs
    elemIsEmpty (Hole _) = False
    elemIsEmpty (Horz xs) = all isEmpty xs
    elemIsEmpty (Vert xs) = all isEmpty xs
    elemIsEmpty (Parn _) = False
    elemIsEmpty (PrHz _) = False

infixr 6 <+> 

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

text' :: ClassNames -> String -> Inlines
text' cs s = Inlines $ Seq.singleton $ Text s cs

-- When there's only 1 Horz inside a Parn, convert it to PrHz
parens :: Inlines -> Inlines
parens (Inlines (Horz xs :<| Empty)) = Inlines $ Seq.singleton $ PrHz xs
parens others = Inlines $ Seq.singleton $ Parn others

icon :: String -> Inlines
icon s = Inlines $ Seq.singleton $ Icon s []

linkRange :: Agda.Range -> Inlines -> Inlines
linkRange range xs = Inlines $ Seq.singleton $ Link range xs mempty

linkHole :: Int -> Inlines
linkHole i = Inlines $ Seq.singleton $ Hole i

--------------------------------------------------------------------------------

type ClassNames = [String]

--------------------------------------------------------------------------------

-- | Internal type, to be converted to JSON values
data Inline
  = Icon String ClassNames
  | Text String ClassNames
  | Link Agda.Range Inlines ClassNames
  | Hole Int
  | -- | Horizontal grouping, wrap when there's no space
    Horz [Inlines]
  | -- | Vertical grouping, each children would end with a newline
    Vert [Inlines]
  | -- | Parenthese
    Parn Inlines
  | -- | Parenthese around a Horizontal, special case 
    PrHz [Inlines]
  deriving (Generic)

instance ToJSON Inline

instance Show Inline where
  show (Icon s _) = s
  show (Text s _) = s
  show (Link _ xs _) = mconcat (map show $ toList $ unInlines xs)
  show (Hole i) = "?" ++ show i
  show (Horz xs) = unwords (map show $ toList xs)
  show (Vert xs) = unlines (map show $ toList xs)
  show (Parn x) = "(" <> show x <> ")" 
  show (PrHz xs) = "(" <> unwords (map show $ toList xs) <> ")" 

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

-- -- TODO: implement this
-- indent :: Inlines -> Inlines
-- indent x = "  " <> x

punctuate :: Inlines -> [Inlines] -> [Inlines]
punctuate _ [] = []
punctuate delim xs = zipWith (<>) xs (replicate (length xs - 1) delim ++ [mempty])

--------------------------------------------------------------------------------

-- | Just pure concatenation, no grouping or whatsoever
hcat :: [Inlines] -> Inlines
hcat = mconcat

hsep :: [Inlines] -> Inlines
hsep [] = mempty
hsep [x] = x
hsep (x : xs) = x <+> hsep xs

--------------------------------------------------------------------------------

-- | Vertical listing
vcat :: [Inlines] -> Inlines
vcat = Inlines . pure . Vert

-- | Horizontal listing
sep :: [Inlines] -> Inlines
sep = Inlines . pure . Horz

fsep :: [Inlines] -> Inlines
fsep = sep

fcat :: [Inlines] -> Inlines
fcat = sep

--------------------------------------------------------------------------------

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
