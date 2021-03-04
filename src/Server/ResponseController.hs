-- |
-- Makes sure that all dispatched works are done.
-- Notify when all dispatched works are done.
module Server.ResponseController where

import Control.Concurrent
import Control.Concurrent.SizedChan
import Control.Monad (void, when)
import Data.IORef

data ResponseController = ResponseController
  { -- | The number of work dispatched
    dispatchedCount :: IORef Int,
    -- | The number of work completed
    completedCount :: IORef Int,
    -- | A channel of "Checkpoints" to be met
    checkpointChan :: SizedChan Checkpoint
  }

-- | An "Checkpoint" is just a number with a callback, the callback will be invoked once the number is "met"
type Checkpoint = (Int, () -> IO ())

-- | Constructs a new ResponseController
new :: IO ResponseController
new =
  ResponseController
    <$> newIORef 0
    <*> newIORef 0
    <*> newSizedChan

-- | Returns a callback, invoked the callback to signal completion.
-- This function and the returned callback are both non-blocking.
dispatch :: ResponseController -> IO (() -> IO ())
dispatch controller = do
  -- bump `dispatchedCount`
  modifyIORef' (dispatchedCount controller) succ
  return $ \() -> do
    -- work completed, bump `completedCount`
    modifyIORef' (completedCount controller) succ

    -- see if there's any Checkpoint
    result <- tryPeekSizedChan (checkpointChan controller)
    case result of
      -- no checkpoints, do nothing
      Nothing -> return ()
      -- a checkpoint is set!
      Just (dispatched, callback) -> do
        completed <- readIORef (completedCount controller)
        -- see if the checkpoint is met
        when (dispatched == completed) $ do
          -- invoke the callback and remove the checkpoint
          callback ()
          void $ readSizedChan (checkpointChan controller)

-- | Expects a callback, which will be invoked once all works dispatched BEFORE have been completed
-- This function is non-blocking
setCheckpoint :: ResponseController -> (() -> IO ()) -> IO ()
setCheckpoint controller callback = do
  dispatched <- readIORef (dispatchedCount controller)
  completed <- readIORef (completedCount controller)
  -- see if the previously dispatched works have been completed
  if dispatched == completed
    then callback ()
    else do
      -- constructs a Checkpoint from `dispatchedCount`
      let checkpoint = (dispatched, callback)
      -- write it to the channel
      writeSizedChan (checkpointChan controller) checkpoint

-- | The blocking version of `setCheckpoint`
setCheckpointAndWait :: ResponseController -> IO ()
setCheckpointAndWait controller = do
  mvar <- newEmptyMVar
  setCheckpoint controller (putMVar mvar)
  takeMVar mvar