-- |
-- Makes sure that all dispatched works are done.
-- Notify when all dispatched works are done.
--
-- All of the state (both counts and the pending checkpoints) is guarded by one
-- `MVar`, so checking whether a checkpoint is met and recording it happen in
-- the same critical section as a completion. Otherwise a completion could land
-- between the check and the write, and the checkpoint would never fire (#78).
module Server.ResponseController
  ( ResponseController,
    new,
    dispatch,
    setCheckpointAndWait,
  )
where

import Control.Concurrent
import Control.Monad (when)
import Data.List (partition)

newtype ResponseController = ResponseController (MVar State)

data State = State
  { -- | The number of work dispatched
    dispatched :: !Int,
    -- | The number of work completed
    completed :: !Int,
    -- | Checkpoints yet to be met
    checkpoints :: [Checkpoint]
  }

-- | An "Checkpoint" is just a number with a callback, the callback will be invoked once the number is "met"
type Checkpoint = (Int, () -> IO ())

-- | Constructs a new ResponseController
new :: IO ResponseController
new = ResponseController <$> newMVar (State 0 0 [])

-- | Returns a callback, invoked the callback to signal completion.
-- This function and the returned callback are both non-blocking.
dispatch :: ResponseController -> IO (() -> IO ())
dispatch (ResponseController var) = do
  -- bump `dispatched`
  modifyMVar_ var $ \s -> return s {dispatched = dispatched s + 1}
  return $ \() -> do
    -- work completed: bump `completed` and take every checkpoint it meets
    met <- modifyMVar var $ \s -> do
      let completed' = completed s + 1
          (reached, pending) = partition (\(target, _) -> target <= completed') (checkpoints s)
      return (s {completed = completed', checkpoints = pending}, reached)
    -- invoke the callbacks outside the lock
    mapM_ (\(_, callback) -> callback ()) met

-- | Expects a callback, which will be invoked once all works dispatched BEFORE have been completed
-- This function is non-blocking
setCheckpoint :: ResponseController -> (() -> IO ()) -> IO ()
setCheckpoint (ResponseController var) callback = do
  runNow <- modifyMVar var $ \s ->
    -- see if the previously dispatched works have been completed
    if dispatched s == completed s
      then return (s, True)
      else -- record a Checkpoint at the current `dispatched`
        return (s {checkpoints = checkpoints s ++ [(dispatched s, callback)]}, False)
  -- invoke the callback outside the lock
  when runNow $ callback ()

-- | The blocking version of `setCheckpoint`
setCheckpointAndWait :: ResponseController -> IO ()
setCheckpointAndWait controller = do
  mvar <- newEmptyMVar
  setCheckpoint controller (putMVar mvar)
  takeMVar mvar
