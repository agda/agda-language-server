module Test.Indexer.NoAnonFunSymbol (tests) where

import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Utils.Lens ((^.))
import Agda.Utils.Maybe (whenJust)
import Data.Foldable (find)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import Server.Model.AgdaFile (agdaFileSymbols)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import qualified TestData

tests :: TestTree
tests =
  testCase "No symbols for internal anonymous function names" $ do
    TestData.AgdaFileDetails name agdaFile interface <- TestData.agdaFileDetails "test/data/AnonFun.agda"

    let symbolNames = prettyShow <$> Map.keys (agdaFile ^. agdaFileSymbols)
    whenJust (find isAnonFunName symbolNames) $ \name ->
      assertFailure $ "Found symbol for internal anonymous function: " ++ name

isAnonFunName :: String -> Bool
isAnonFunName name = ".extendedlambda" `isInfixOf` name
