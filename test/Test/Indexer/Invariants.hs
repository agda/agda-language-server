module Test.Indexer.Invariants (tests) where

import Control.Monad (forM)
import Test.Indexer.Invariants.NoDuplicateDecl (testNoDuplicateDecl)
import Test.Indexer.Invariants.NoMissing (testNoMissing)
import Test.Indexer.Invariants.NoOverlap (testNoOverlap)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import qualified TestData

tests :: IO TestTree
tests = do
  inPaths <- findByExtension [".agda"] "test/data/Indexer"
  files <- forM inPaths $ \inPath -> do
    TestData.AgdaFileDetails testName file interface <- TestData.agdaFileDetails inPath
    return (testName, file, interface)

  return $
    testGroup
      "Invariants"
      [ testGroup "No reference overlap" ((\(name, file, _interface) -> testNoOverlap name file) <$> files),
        testGroup "No missing references" ((\(name, file, interface) -> testNoMissing name file interface) <$> files),
        testGroup "No duplicate declarations" ((\(name, file, _interface) -> testNoDuplicateDecl name file) <$> files)
      ]
