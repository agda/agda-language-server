module Agda.Position
  ( OffsetTable,
    makeOffsetTable,
    lookupOffsetTable,
    toPositionWithoutFile,
    toRange,
    prettyPositionWithoutFile,
  )
where

import Agda.Syntax.Position
import Agda.Utils.FileName (AbsolutePath (AbsolutePath))
import qualified Data.Sequence as Seq
import qualified Data.Strict.Maybe as Strict
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP

-- Note:  LSP srclocs are 0-base
--        Agda srclocs are 1-base

--------------------------------------------------------------------------------

-- | LSP locations -> Agda locations

-- | LSP Range -> Agda Range
toRange :: OffsetTable -> Text -> LSP.Range -> Range
toRange table path (LSP.Range start end) =
  Range (Strict.Just (AbsolutePath path)) (Seq.singleton interval)
  where
    interval :: IntervalWithoutFile
    interval = Interval (toPositionWithoutFile table start) (toPositionWithoutFile table end)

-- | LSP Position -> Agda PositionWithoutFile
toPositionWithoutFile :: OffsetTable -> LSP.Position -> PositionWithoutFile
toPositionWithoutFile table (LSP.Position line col) =
  Pn
    ()
    (fromIntegral (lookupOffsetTable table line col) + 1)
    (fromIntegral line + 1)
    (fromIntegral col + 1)

prettyPositionWithoutFile :: PositionWithoutFile -> String
prettyPositionWithoutFile pos@(Pn () offset line col) = "[" <> show pos <> "-" <> show offset <> "]"

--------------------------------------------------------------------------------

-- | Positon -> Offset convertion

-- | A list of offsets of linebreaks ("\n", "\r" or "\r\n")
newtype OffsetTable = OffsetTable [Int]

data Accum = Accum
  { accumPreviousChar :: Maybe Char,
    accumCurrentOffset :: Int,
    accumOffsetTable :: [Int]
  }

initAccum :: Accum
initAccum = Accum Nothing 0 [0]

-- | Return a list of offsets of linebreaks ("\n", "\r" or "\r\n")
makeOffsetTable :: Text -> OffsetTable
makeOffsetTable = OffsetTable . reverse . accumOffsetTable . Text.foldl' go initAccum
  where
    go :: Accum -> Char -> Accum
    go (Accum (Just '\r') n []) '\n' = Accum (Just '\n') (1 + n) [1] -- impossible case
    go (Accum (Just '\r') n (m : acc)) '\n' = Accum (Just '\n') (1 + n) (1 + m : acc)
    go (Accum previous n acc) '\n' = Accum (Just '\n') (1 + n) (1 + n : acc)
    go (Accum previous n acc) '\r' = Accum (Just '\r') (1 + n) (1 + n : acc)
    go (Accum previous n acc) char = Accum (Just char) (1 + n) acc

-- | Zero-based
lookupOffsetTable :: OffsetTable -> Int -> Int -> Int
lookupOffsetTable (OffsetTable offsets) line col = offsets !! line + col

-- toLSPRange :: Range -> LSP.Range
-- toLSPRange range = case rangeToIntervalWithFile range of
--   Nothing -> LSP.Range (LSP.Position (-1) (-1)) (LSP.Position (-1) (-1))
--   Just (Interval start end) -> LSP.Range (toLSPPosition start) (toLSPPosition end)

-- toLSPPosition :: Position -> LSP.Position
-- toLSPPosition (Pn _ offset line col) = LSP.Position (fromIntegral line - 1) (fromIntegral col - 1)
