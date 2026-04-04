-- | Check that highlighting data doesn't show more references than we have
-- `Ref`s. We expect to sometimes pick up references that highlighting does not,
-- so it's okay if we have more.
module Test.Indexer.Invariants.NoMissing (testNoMissing) where

import Agda.Interaction.Highlighting.Precise (Aspects, HighlightingInfo)
import qualified Agda.Interaction.Highlighting.Precise as HL
import Agda.Position (FromOffset, fromOffset, makeFromOffset)
import Agda.Syntax.Common.Pretty (Pretty, align, pretty, prettyList, prettyShow, pshow, text, vcat, (<+>))
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.Lens ((^.))
import Agda.Utils.Maybe (isNothing)
import Agda.Utils.RangeMap (PairInt (PairInt), RangeMap (rangeMap))
import Control.Monad (forM_, guard)
import Control.Monad.Writer.CPS (Writer, execWriter, tell)
import qualified Data.Map as Map
import Data.Strict (Pair ((:!:)))
import qualified Data.Strict as Strict
import qualified Language.LSP.Protocol.Types as LSP
import Server.Model.AgdaFile (AgdaFile, agdaFileRefs)
import Server.Model.Symbol (Ref (refRange))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

data MissingRefError = MissingRefError
  { mreAspects :: !Aspects,
    mreRange :: !LSP.Range
  }

instance Pretty MissingRefError where
  pretty err =
    text "Missing ref error at"
      <+> pretty (mreRange err)
      <+> text "with aspects"
      <+> pshow (mreAspects err)

  prettyList = \case
    [] -> text "No missing ref errors"
    [err] -> pretty err
    errs -> align 5 $ (\err -> ("-", vcat [pretty err, text ""])) <$> errs

areAspectsRef :: Aspects -> Bool
areAspectsRef aspects = case HL.aspect aspects of
  Just HL.PrimitiveType -> True
  Just (HL.Name _ _) -> True
  _ -> False

highlightingRefRanges :: FromOffset -> HighlightingInfo -> [(Aspects, LSP.Range)]
highlightingRefRanges table highlightingInfo = do
  (startOffset, PairInt (endOffset :!: aspects)) <- Map.toList $ rangeMap highlightingInfo
  guard $ areAspectsRef aspects
  let (startLine, startChar) = fromOffset table startOffset
      startPos = LSP.Position (fromIntegral startLine) (fromIntegral startChar - 1)
      (endLine, endChar) = fromOffset table (endOffset - 1)
      endPos = LSP.Position (fromIntegral endLine) (fromIntegral endChar)
      range = LSP.Range startPos endPos
  return (aspects, range)

hasCorrespondingRef :: AgdaFile -> Aspects -> LSP.Range -> Bool
hasCorrespondingRef agdaFile aspects range =
  let refs = concat $ Map.elems $ agdaFile ^. agdaFileRefs
   in isNothing (HL.definitionSite aspects)
        || any (\ref -> refRange ref == range) refs

checkCorrespondingRef :: AgdaFile -> Aspects -> LSP.Range -> Writer [MissingRefError] ()
checkCorrespondingRef agdaFile aspects range =
  if hasCorrespondingRef agdaFile aspects range
    then return ()
    else tell $ [MissingRefError aspects range]

testNoMissing :: String -> AgdaFile -> TCM.Interface -> TestTree
testNoMissing name agdaFile interface = testCase name $ do
  let highlighting = TCM.iHighlighting interface
  let table = makeFromOffset $ Strict.toStrict $ TCM.iSource interface
  let highlightingRanges = highlightingRefRanges table highlighting

  let errors = execWriter $ forM_ highlightingRanges $ \(aspects, range) ->
        checkCorrespondingRef agdaFile aspects range

  case errors of
    [] -> return ()
    _ -> assertFailure $ prettyShow errors
