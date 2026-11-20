module Test.ResponseController (tests) where

import Control.Concurrent
import Server.ResponseController (dispatch, new, setCheckpointAndWait)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ResponseController"
    [ testCase "checkpoint fires when the last answer races setCheckpointAndWait" raceTest,
      testCase "checkpoint fires when the answer comes before setCheckpointAndWait" $ do
        c <- new
        done <- dispatch c
        done ()
        result <- timeout oneSecond (setCheckpointAndWait c)
        result @?= Just (),
      testCase "checkpoint fires when the answer comes after the checkpoint is set" $ do
        c <- new
        done <- dispatch c
        finished <- newEmptyMVar
        _ <- forkIO (setCheckpointAndWait c >> putMVar finished ())
        threadDelay 10000
        done ()
        result <- timeout oneSecond (takeMVar finished)
        result @?= Just ()
    ]

oneSecond :: Int
oneSecond = 1000000

-- | Issue #78: the only response's answer races the checkpoint. Once the answer
-- is in, the waiter must return; if it stays blocked, the hang has reproduced.
raceTest :: Assertion
raceTest = go 0
  where
    iterations :: Int
    iterations = 200000

    go :: Int -> Assertion
    go i
      | i >= iterations = return ()
      | otherwise = do
          c <- new
          done <- dispatch c
          finished <- newEmptyMVar
          waiter <- forkIO (setCheckpointAndWait c >> putMVar finished ())
          case i `mod` 3 of
            0 -> return ()
            1 -> yield
            _ -> threadDelay 1
          done ()
          result <- timeout oneSecond (takeMVar finished)
          case result of
            Just () -> go (i + 1)
            Nothing -> do
              killThread waiter
              assertFailure $
                "iteration "
                  <> show i
                  <> ": the only response was answered, but setCheckpointAndWait is still blocked"
