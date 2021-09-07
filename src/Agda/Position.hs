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
  (fromIntegral (toOffset table (line, col)) + 1)
  (fromIntegral line + 1)
  (fromIntegral col + 1)

prettyPositionWithoutFile :: PositionWithoutFile -> String
prettyPositionWithoutFile pos@(Pn () offset _line _col) =
  "[" <> show pos <> "-" <> show offset <> "]"

--------------------------------------------------------------------------------
-- | Positon => Offset convertion

-- | TODO: implement ToOffset with IntMap instead 
newtype ToOffset = ToOffset [Int]

data AccumToOffset = AccumT
  { accumPreviousCharT  :: Maybe Char
  , accumCurrentOffsetT :: Int
  , accumResultT        :: [Int]
  }

-- | Return a list of offsets of linebreaks ("\n", "\r" or "\r\n")
makeToOffset :: Text -> ToOffset
makeToOffset = ToOffset . reverse . accumResultT . Text.foldl' go initAccum
 where
  initAccum :: AccumToOffset
  initAccum = AccumT Nothing 0 [0]

  go :: AccumToOffset -> Char -> AccumToOffset
  go (AccumT (Just '\r') n []) '\n' = AccumT (Just '\n') (1 + n) [1] -- impossible case
  go (AccumT (Just '\r') n (m : acc)) '\n' =
    AccumT (Just '\n') (1 + n) (1 + m : acc)
  go (AccumT previous n acc) '\n' = AccumT (Just '\n') (1 + n) (1 + n : acc)
  go (AccumT previous n acc) '\r' = AccumT (Just '\r') (1 + n) (1 + n : acc)
  go (AccumT previous n acc) char = AccumT (Just char) (1 + n) acc

-- | (line, col) => offset (zero-based)
toOffset :: ToOffset -> (Int, Int) -> Int
toOffset (ToOffset offsets) (line, col) = offsets !! line + col

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
newtype FromOffset = FromOffset {unFromOffset :: IntMap Int }

fromOffset :: FromOffset -> Int -> (Int, Int)
fromOffset (FromOffset table) offset = case IntMap.lookupLE offset table of
  Nothing                          -> (0, offset) -- no previous lines
  Just (offsetOfFirstChar, lineNo) -> (lineNo, offset - offsetOfFirstChar)

makeFromOffset :: Text -> FromOffset
makeFromOffset = FromOffset . accumResultF . Text.foldl'
  go
  (AccumF Nothing 0 0 IntMap.empty)
 where
  go :: AccumFromOffset -> Char -> AccumFromOffset
  -- encountered a "\r\n", update the latest entry 
  go (AccumF (Just '\r') n l table) '\n' = case IntMap.deleteFindMax table of
    ((offset, lineNo), table') ->
      AccumF (Just '\n') (1 + n) l (IntMap.insert (1 + offset) lineNo table')
  -- encountered a line break, add a new entry 
  go (AccumF previous n l table) '\n' =
    AccumF (Just '\n') (1 + n) (1 + l) (IntMap.insert (1 + n) (1 + l) table)
  go (AccumF previous n l table) '\r' =
    AccumF (Just '\r') (1 + n) (1 + l) (IntMap.insert (1 + n) (1 + l) table)
  go (AccumF previous n l table) char = AccumF (Just char) (1 + n) l table

data AccumFromOffset = AccumF
  { accumPreviousCharF  :: Maybe Char
  , accumCurrentOffsetF :: Int
  , accumCurrentLineF   :: Int
  , accumResultF        :: IntMap Int
  }

-- --------------------------------------------------------------------------------
-- -- | Agda Highlighting Range -> Agda Range

-- fromAgdaHighlightingRangeToLSPRange :: Range -> LSP.Range
-- fromAgdaHighlightingRangeToLSPRange range = case rangeToIntervalWithFile range of
--   Nothing -> LSP.Range (LSP.Position (-1) (-1)) (LSP.Position (-1) (-1))
--   Just (Interval start end) -> LSP.Range (toLSPPosition start) (toLSPPosition end)

-- toLSPPosition :: Position -> LSP.Position
-- toLSPPosition (Pn _ offset line col) = LSP.Position (fromIntegral line - 1) (fromIntegral col - 1)
