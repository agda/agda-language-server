module Test.SrcLoc where

import Agda.Position
import qualified Data.IntMap as IntMap
import Data.List (sort)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Source Location" [positionToOffsetTests, offsetToPositionTests]

--------------------------------------------------------------------------------

positionToOffsetTests :: TestTree
positionToOffsetTests =
  testGroup
    "Position => Offset"
    [ testCase "cached table" $ IntMap.toList (unToOffset table) @?= [(1, 4), (2, 9), (3, 12)],
      testCase "line 0" $ run [(0, 0), (0, 1), (0, 2), (0, 3)] @?= [0, 1, 2, 3],
      testCase "line 1" $ run [(1, 0), (1, 1), (1, 2), (1, 3), (1, 4)] @?= [4, 5, 6, 7, 8],
      testCase "line 2" $ run [(2, 0), (2, 1)] @?= [9, 10],
      testCase "line 3" $ run [(3, 0), (3, 1)] @?= [12, 13]
    ]
  where
    text :: Text
    text = "012\n456\r\n90\r23"

    table :: ToOffset
    table = makeToOffset text

    run :: [(Int, Int)] -> [Int]
    run = map (toOffset table)

--------------------------------------------------------------------------------

offsetToPositionTests :: TestTree
offsetToPositionTests =
  testGroup
    "Offset => Position"
    [ testCase "cached table" $ IntMap.toList (unFromOffset table) @?= [(4, 1), (9, 2), (12, 3)],
      testCase "line 0" $ run [0, 1, 2, 3] @?= [(0, 0), (0, 1), (0, 2), (0, 3)],
      testCase "line 1" $ run [4, 5, 6, 7, 8] @?= [(1, 0), (1, 1), (1, 2), (1, 3), (1, 4)],
      testCase "line 2" $ run [9, 10] @?= [(2, 0), (2, 1)],
      testCase "line 3" $ run [12, 13] @?= [(3, 0), (3, 1)]
    ]
  where
    text :: Text
    text = "012\n456\r\n90\r23"

    table :: FromOffset
    table = makeFromOffset text

    run :: [Int] -> [(Int, Int)]
    run = map (fromOffset table)
