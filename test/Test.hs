module Test where

import qualified Test.SrcLoc                   as SrcLoc
import qualified Test.LSP                   as LSP
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [SrcLoc.tests,
        LSP.tests
    ]
