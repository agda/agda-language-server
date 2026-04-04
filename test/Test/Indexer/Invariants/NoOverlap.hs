-- | Check that `Ref`s don't overlap, so that there is only one `Ref` instance
-- per actual reference.
module Test.Indexer.Invariants.NoOverlap (testNoOverlap) where

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (Doc, Pretty (prettyList), align, pretty, prettyShow, text, vcat, (<+>))
import Agda.Utils.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Writer.CPS (Writer, execWriter, tell)
import Data.Foldable (foldrM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Server.Model.AgdaFile (AgdaFile, agdaFileRefs)
import Server.Model.Symbol (Ref (refRange), refIsAmbiguous)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

data OverlapError = OverlapError
  { oeLineNum :: !Int,
    oePart1 :: !LinePart,
    oePart2 :: !LinePart
  }

instance Pretty OverlapError where
  pretty err =
    vcat
      [ text "Overlap error at line number"
          <+> pretty (oeLineNum err + 1)
          <+> linePartPrettyRange (oePart1 err)
          <+> text "and"
          <+> linePartPrettyRange (oePart2 err),
        pretty (linePartName (oePart1 err))
          <+> pretty (linePartRef (oePart1 err)),
        pretty (linePartName (oePart2 err))
          <+> pretty (linePartRef (oePart2 err))
      ]

  prettyList = \case
    [] -> text "No overlap errors"
    [err] -> pretty err
    errs -> align 5 $ (\err -> ("-", vcat [pretty err, text ""])) <$> errs

type OverlapResult = Writer [OverlapError]

data LinePart
  = RangePart !A.QName !Ref !Int !Int
  | ToEndPart !A.QName !Ref !Int

linePartPrettyRange :: LinePart -> Doc
linePartPrettyRange (RangePart _ _ start end) = text "from" <+> pretty start <+> text "to" <+> pretty end
linePartPrettyRange (ToEndPart _ _ start) = text "from" <+> pretty start <+> text "to the end of the line"

linePartName :: LinePart -> A.QName
linePartName (RangePart name _ _ _) = name
linePartName (ToEndPart name _ _) = name

linePartRef :: LinePart -> Ref
linePartRef (RangePart _ ref _ _) = ref
linePartRef (ToEndPart _ ref _) = ref

assertPartsNonoverlapping :: Int -> LinePart -> LinePart -> OverlapResult ()
assertPartsNonoverlapping lineNum part1 part2 = case (part1, part2) of
  -- Allow refs known to be ambiguous
  (_, _)
    | refIsAmbiguous (linePartRef part1)
        && refIsAmbiguous (linePartRef part2) ->
        return ()
  (ToEndPart _ _ _, ToEndPart _ _ _) -> err
  (RangePart _ _ _ rangeEnd, ToEndPart _ _ toEndStart)
    | rangeEnd > toEndStart -> err
  (ToEndPart _ _ toEndStart, RangePart _ _ _ rangeEnd)
    | rangeEnd > toEndStart -> err
  (RangePart _ _ start1 end1, RangePart _ _ start2 end2)
    | (start1 <= start2 && end1 > start2)
        || (start2 <= start1 && end2 > start1) ->
        err
  _ -> return ()
  where
    err = tell $ [OverlapError lineNum part1 part2]

newtype Line = Line [LinePart]

rangePart :: (Integral n) => A.QName -> Ref -> n -> n -> Line
rangePart name ref start end = Line [RangePart name ref (fromIntegral start) (fromIntegral end)]

toEndPart :: (Integral n) => A.QName -> Ref -> n -> Line
toEndPart name ref start = Line [ToEndPart name ref (fromIntegral start)]

tryInsertPart :: Int -> LinePart -> Line -> OverlapResult (Line)
tryInsertPart lineNum part (Line parts) = do
  forM_ parts $ \part2 ->
    assertPartsNonoverlapping lineNum part part2
  return $ Line $ part : parts

tryMerge :: Int -> Line -> Line -> OverlapResult (Line)
tryMerge lineNum (Line newParts) line = foldrM (tryInsertPart lineNum) line newParts

rangeToLines :: A.QName -> Ref -> LSP.Range -> [(Int, Line)]
rangeToLines name ref range =
  if startLine == endLine
    then
      [ ( startLine,
          rangePart
            name
            ref
            (range ^. LSP.start . LSP.character)
            (range ^. LSP.end . LSP.character)
        )
      ]
    else
      let start = (startLine, toEndPart name ref $ range ^. LSP.start . LSP.character)
          end = (endLine, rangePart name ref 0 $ range ^. LSP.end . LSP.character)
          middle = do
            lineNum <- [startLine + 1 .. endLine - 1]
            return (lineNum, toEndPart name ref 0)
       in [start] <> middle <> [end]
  where
    startLine = fromIntegral $ range ^. LSP.start . LSP.line
    endLine = fromIntegral $ range ^. LSP.end . LSP.line

newtype FileRanges = FileRanges (IntMap (Line))

emptyFileRanges :: FileRanges
emptyFileRanges = FileRanges IntMap.empty

tryInsertLine :: Int -> Line -> FileRanges -> OverlapResult (FileRanges)
tryInsertLine lineNum line (FileRanges ranges) =
  FileRanges <$> IntMap.alterF helper lineNum ranges
  where
    helper Nothing = return $ Just line
    helper (Just oldLine) = Just <$> tryMerge lineNum line oldLine

tryInsertRange :: A.QName -> Ref -> LSP.Range -> FileRanges -> OverlapResult (FileRanges)
tryInsertRange name ref range fileRanges = do
  let lines = rangeToLines name ref range
  foldrM (uncurry tryInsertLine) fileRanges lines

testNoOverlap :: String -> AgdaFile -> TestTree
testNoOverlap name file = testCase name $ do
  let refs =
        concatMap (\(name, refs) -> (\ref -> (name, ref)) <$> refs) $
          Map.toList $
            file ^. agdaFileRefs
  let fileRanges = emptyFileRanges
  let errors =
        execWriter $
          foldrM (\(name, ref) -> tryInsertRange name ref (refRange ref)) fileRanges refs

  case errors of
    [] -> return ()
    _ -> assertFailure $ prettyShow errors
