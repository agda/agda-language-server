module Test.Version (tests) where

import Data.Version (Version (..))
import Options (versionNumber)
import qualified Paths_agda_language_server as Paths
import Test.Tasty
import Test.Tasty.HUnit

-- | Guards against the two version numbers drifting apart again, as
-- happened before #56: 'package.yaml' was bumped without updating
-- 'versionNumber' in "Options", so the reported LSP/CLI version fell
-- behind the package version.
tests :: TestTree
tests =
  testGroup
    "Version"
    [ testCase "versionNumber matches package.yaml's version" $
        Version [versionNumber] [] @?= Paths.version
    ]
