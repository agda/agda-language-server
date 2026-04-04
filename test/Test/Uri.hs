module Test.Uri (tests) where

import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.Uri.More (uriHeightAbove)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "URI" $
    [ testGroup "uriHeightAbove" $
        [ testCase "calculates positive height" $ do
            let ancestor = LSP.toNormalizedUri $ LSP.Uri "https://example.com/a/b/c/d/"
            let descendant = LSP.toNormalizedUri $ LSP.Uri "https://example.com/a/b/c/d/e/f"
            let height = uriHeightAbove ancestor descendant
            height @?= 2,
          testCase "calculates 0 height" $ do
            let ancestor = LSP.toNormalizedUri $ LSP.Uri "file://home/user/username/a/"
            let descendant = LSP.toNormalizedUri $ LSP.Uri "file://home/user/username/a"
            let height = uriHeightAbove ancestor descendant
            height @?= 0
        ]
    ]
