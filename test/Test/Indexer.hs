module Test.Indexer (tests) where

import qualified Test.Indexer.Invariants as Invariants
import qualified Test.Indexer.NoAnonFunSymbol as NoAnonFunSymbol
import qualified Test.Indexer.Reload as Reload
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  invariantsTests <- Invariants.tests
  return $
    testGroup "Indexer" $
      [ invariantsTests,
        NoAnonFunSymbol.tests,
        Reload.tests
      ]
