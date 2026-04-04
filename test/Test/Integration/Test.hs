module Test.Integration.Test (tests) where

import qualified Test.Integration.LibraryDependencies.Test as LibraryDependencies
import Test.Tasty (TestTree, testGroup)

tests :: FilePath -> TestTree
tests alsPath =
  testGroup
    "Integration tests"
    [ LibraryDependencies.test alsPath
    ]
