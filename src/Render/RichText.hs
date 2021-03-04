{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.RichText
  ( RichText (..),
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
    forallQ,
    showIndex,
    leftIdiomBrkt,
    rightIdiomBrkt,
    emptyIdiomBrkt,
  )
where

import qualified Agda.Interaction.Options.IORefs as Agda
import qualified Agda.Syntax.Common as Agda
import qualified Agda.Syntax.Position as Agda
import qualified Agda.Utils.FileName as Agda
import qualified Agda.Utils.Null as Agda
import Agda.Utils.Pretty (Doc)
import qualified Agda.Utils.Pretty as Doc
import Agda.Utils.Suffix (subscriptAllowed, toSubscriptDigit)
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

newtype RichText = RichText (Seq Inline)

-- Represent RichText with String literals
instance IsString RichText where
  fromString s = RichText (Seq.singleton (Text s mempty))

instance Semigroup RichText where
  RichText xs <> RichText ys = RichText (xs <> ys)

instance Monoid RichText where
  mempty = RichText mempty

instance ToJSON RichText where
  toJSON (RichText xs) = toJSON xs

instance Show RichText where
  show (RichText xs) = unwords $ map show $ toList xs

-- | An alternative @show@ for debugging 
-- debug :: RichText -> String 
-- debug (RichText xs) = show $ toList $ fmap (\(Elem s a) -> s <> " " <> show a) xs

(<+>) :: RichText -> RichText -> RichText
x <+> y = x <> " " <> y

-- | Whitespace
space :: RichText
space = " "

text :: String -> RichText
text s = RichText $ Seq.singleton $ Text s mempty

icon :: String -> RichText
icon s = RichText $ Seq.singleton $ Icon s []

linkRange :: Agda.Range -> RichText -> RichText
linkRange range (RichText xs) = RichText $ Seq.singleton $ Link range (toList xs) mempty 

linkHole :: Int -> RichText -> RichText
linkHole i (RichText xs) = RichText $ Seq.singleton $ Hole i

--------------------------------------------------------------------------------

type ClassNames = [String]
--------------------------------------------------------------------------------

-- | Internal type, to be converted to JSON values
data Inline 
  = Icon String ClassNames
  | Text String ClassNames
  | Link Agda.Range [Inline] ClassNames
  | Hole Int
  deriving (Generic)

instance ToJSON Inline

instance Show Inline where
  show (Icon s _) = s
  show (Text s _) = s
  show (Link _ s _) = mconcat $ fmap show s
  show (Hole i) = "?" ++ show i

addClassName :: ClassNames -> Inline -> Inline
addClassName ys elem = case elem of 
  Icon s xs -> Icon s (xs <> ys)
  Text s xs -> Text s (xs <> ys)
  Link range s xs -> Link range s (xs <> ys)
  Hole i    -> Hole i

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
sepBy delim [x] = x
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

-- left, right, and empty idiom bracket
leftIdiomBrkt, rightIdiomBrkt, emptyIdiomBrkt :: RichText
leftIdiomBrkt = _leftIdiomBrkt specialCharacters
rightIdiomBrkt = _rightIdiomBrkt specialCharacters
emptyIdiomBrkt = _emptyIdiomBrkt specialCharacters

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
