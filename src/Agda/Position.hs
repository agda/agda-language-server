module Agda.Position
  ( ToOffset(..)
  , makeToOffset
  , toOffset
  , FromOffset(..)
  , makeFromOffset
  , fromOffset
  , toAgdaPositionWithoutFile
  , toAgdaRange
  , prettyPositionWithoutFile
  -- , toLSPRange
  -- , toLSPPosition
  ) where

import           Agda.Syntax.Position
import           Agda.Utils.FileName            ( AbsolutePath(AbsolutePath) )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import qualified Data.Sequence                 as Seq
import qualified Data.Strict.Maybe             as Strict
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Language.LSP.Types            as LSP

-- Note:  LSP srclocs are 0-base
--        Agda srclocs are 1-base

--------------------------------------------------------------------------------
-- | LSP source locations => Agda source locations

-- | LSP Range -> Agda Range
toAgdaRange :: ToOffset -> Text -> LSP.Range -> Range
toAgdaRange table path (LSP.Range start end) = Range
  (Strict.Just (AbsolutePath path))
  (Seq.singleton interval)
 where
  interval :: IntervalWithoutFile
  interval = Interval (toAgdaPositionWithoutFile table start)
                      (toAgdaPositionWithoutFile table end)

-- | LSP Position -> Agda PositionWithoutFile
toAgdaPositionWithoutFile :: ToOffset -> LSP.Position -> PositionWithoutFile
toAgdaPositionWithoutFile table (LSP.Position line col) = Pn
  ()
  (fromIntegral (toOffset table (fromIntegral line, fromIntegral col)) + 1)
  (fromIntegral line + 1)
  (fromIntegral col + 1)

prettyPositionWithoutFile :: PositionWithoutFile -> String
prettyPositionWithoutFile pos@(Pn () offset _line _col) =
  "[" <> show pos <> "-" <> show offset <> "]"

--------------------------------------------------------------------------------
-- | Positon => Offset convertion

-- Keeps record of offsets of every line break ("\n", "\r" and "\r\n")
--
--  Example text      corresponding entry of IntMap
--  >abc\n               (1, 4)
--  >def123\r\n          (2, 11)
--  >ghi\r               (3, 15)
--
newtype ToOffset = ToOffset { unToOffset :: IntMap Int }

data Accum = Accum
  { accumPreviousChar  :: Maybe Char
  , accumCurrentOffset :: Int
  , accumCurrentLine   :: Int
  , accumResult        :: IntMap Int
  }

-- | Return a list of offsets of linebreaks ("\n", "\r" or "\r\n")
makeToOffset :: Text -> ToOffset
makeToOffset = ToOffset . accumResult . Text.foldl' go initAccum
 where
  initAccum :: Accum
  initAccum = Accum Nothing 0 0 IntMap.empty

  go :: Accum -> Char -> Accum
  go (Accum (Just '\r') n l table) '\n' =
    Accum (Just '\n') (1 + n) l (IntMap.updateMax (Just . succ) table)
  go (Accum previous n l table) '\n' =
    Accum (Just '\n') (1 + n) (1 + l) (IntMap.insert (1 + l) (1 + n) table)
  go (Accum previous n l table) '\r' =
    Accum (Just '\r') (1 + n) (1 + l) (IntMap.insert (1 + l) (1 + n) table)
  go (Accum previous n l table) char = Accum (Just char) (1 + n) l table

-- | (line, col) => offset (zero-based)
toOffset :: ToOffset -> (Int, Int) -> Int
toOffset (ToOffset table) (line, col) = case IntMap.lookup line table of
  Nothing     -> col
  Just offset -> offset + col

--------------------------------------------------------------------------------
-- | Offset => Position convertion

-- An IntMap for speeding up Offset => Position convertion
-- Keeps record of offsets of every line break ("\n", "\r" and "\r\n")
--
--  Example text      corresponding entry of IntMap
--  >abc\n               (4, 1)
--  >def123\r\n          (11, 2)
--  >ghi\r               (15, 3)
--
newtype FromOffset = FromOffset { unFromOffset :: IntMap Int }

fromOffset :: FromOffset -> Int -> (Int, Int)
fromOffset (FromOffset table) offset = case IntMap.lookupLE offset table of
  Nothing                          -> (0, offset) -- no previous lines
  Just (offsetOfFirstChar, lineNo) -> (lineNo, offset - offsetOfFirstChar)

makeFromOffset :: Text -> FromOffset
makeFromOffset = FromOffset . accumResult . Text.foldl'
  go
  (Accum Nothing 0 0 IntMap.empty)
 where
  go :: Accum -> Char -> Accum
  -- encountered a "\r\n", update the latest entry
  go (Accum (Just '\r') n l table) '\n' = case IntMap.deleteFindMax table of
    ((offset, lineNo), table') ->
      Accum (Just '\n') (1 + n) l (IntMap.insert (1 + offset) lineNo table')
  -- encountered a line break, add a new entry
  go (Accum previous n l table) '\n' =
    Accum (Just '\n') (1 + n) (1 + l) (IntMap.insert (1 + n) (1 + l) table)
  go (Accum previous n l table) '\r' =
    Accum (Just '\r') (1 + n) (1 + l) (IntMap.insert (1 + n) (1 + l) table)
  go (Accum previous n l table) char = Accum (Just char) (1 + n) l table

-- --------------------------------------------------------------------------------
-- -- | Agda Highlighting Range -> Agda Range

-- fromAgdaHighlightingRangeToLSPRange :: Range -> LSP.Range
-- fromAgdaHighlightingRangeToLSPRange range = case rangeToIntervalWithFile range of
--   Nothing -> LSP.Range (LSP.Position (-1) (-1)) (LSP.Position (-1) (-1))
--   Just (Interval start end) -> LSP.Range (toLSPPosition start) (toLSPPosition end)

-- toLSPPosition :: Position -> LSP.Position
-- toLSPPosition (Pn _ offset line col) = LSP.Position (fromIntegral line - 1) (fromIntegral col - 1)
