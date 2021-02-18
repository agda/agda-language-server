-- |
-- Makes sure that all dispatched works are done.
-- Notify when all dispatched works are done.
module Control.Concurrent.Foreman where

import Control.Concurrent
import Data.IORef
import Control.Concurrent.SizedChan
import Control.Monad (when, void)

data Foreman = Foreman
  { -- | The number of work dispatched
    dispatchedCount :: IORef Int,
    -- | The number of work completed
    completedCount :: IORef Int,
    -- | A channel of "Goals" to be met
    goalChan :: SizedChan Goal
  }

-- | An "Goal" is just a number with a callback, the callback will be invoked once the number is "met"
type Goal = (Int, () -> IO ())

-- | Constructs a new Foreman
new :: IO Foreman
new =
  Foreman
    <$> newIORef 0
    <*> newIORef 0
    <*> newSizedChan 

-- | Returns a callback, invoked it to signal completion.
-- This function and the returned callback are both non-blocking.
dispatch :: Foreman -> IO (() -> IO ())
dispatch foreman = do
  -- bump `dispatchedCount`
  modifyIORef' (dispatchedCount foreman) succ
  return $ \() -> do
    -- work completed, bump `completedCount`
    modifyIORef' (completedCount foreman) succ

    -- see if there's any Goal 
    result <- tryPeekSizedChan (goalChan foreman)
    case result of 
      -- no goals, do nothing 
      Nothing -> return ()
      -- a goal is set!
      Just (dispatched, callback) -> do 
        completed <- readIORef (completedCount foreman)
        -- see if the goal is met 
        when (dispatched == completed) $ do 
          -- invoke the callback and remove the goal
          callback ()
          void $ readSizedChan (goalChan foreman)

-- | Expects a callback, which will be invoked once all works dispatched BEFORE have been completed 
-- This function is non-blocking
setGoal :: Foreman -> (() -> IO ()) -> IO ()
setGoal foreman callback = do
  dispatched <- readIORef (dispatchedCount foreman)
  completed <- readIORef (completedCount foreman)
  -- see if the previously dispatched works have been completed
  if dispatched == completed 
    then callback ()
    else do 
      -- constructs a Goal from `dispatchedCount`
      let goal = (dispatched, callback)
      -- write it to the channel
      writeSizedChan (goalChan foreman) goal 

-- | The blocking version of `setGoal`
setGoalAndWait :: Foreman -> IO ()
setGoalAndWait foreman = do
  mvar <- newEmptyMVar
  setGoal foreman (putMVar mvar)
  takeMVar mvar