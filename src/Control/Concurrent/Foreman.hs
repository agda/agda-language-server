-- |
-- Makes sure that all dispatched works are done.
-- Notify when all dispatched works are done.
module Control.Concurrent.Foreman where

import Control.Concurrent
import Control.Monad (when)
import Data.IORef

data Foreman = Foreman
  { -- | The number of work dispatched
    dispatchedCount :: IORef Int,
    -- | The number of work completed
    completedCount :: IORef Int,
    -- | When set, will store the current `dispatchedCount` and a callback
    expectedCount :: IORef (Maybe (Int, Int -> IO ()))
  }

-- | Constructs a new Foreman
new :: IO Foreman
new =
  Foreman
    <$> newIORef 0
    <*> newIORef 0
    <*> newIORef Nothing

-- | Invoke the returned callback to signal completion
dispatch :: Foreman -> IO (() -> IO ())
dispatch foreman = do
  -- bump `dispatchedCount`
  modifyIORef' (dispatchedCount foreman) succ
  return $ \() -> do
    -- bump `completedCount`
    modifyIORef' (completedCount foreman) succ
    -- check if `expectedCount` is set
    expected <- readIORef (expectedCount foreman)
    case expected of
      -- `expectedCount` not set, do nothing
      Nothing -> return ()
      -- `expectedCount` is set
      Just (dispached, callback) -> do
        completed <- readIORef (completedCount foreman)
        print (dispached, completed)
        -- fill the MVar to signal completion when the number matched
        when (dispached == completed) $
          callback dispached

-- | The given callback will be invoked with the number of works dispatched, when all of them have been completed
-- This function is non-blocking, and is expected to be called only once.
-- All subsequent calls will be ignored (and the given callback won't be invoked neither)
complete :: Foreman -> (Int -> IO ()) -> IO ()
complete foreman callback = do
  -- check if `expectedCount` is set
  expected <- readIORef (expectedCount foreman)
  case expected of
    -- `expectedCount` is set, ignore this call
    Just (_, _) -> return ()
    -- `expectedCount` is not set
    Nothing -> do
      -- copy `dispatchedCount`
      dispatched <- readIORef (dispatchedCount foreman)
      -- store `dispatchedCount` and the callback
      writeIORef (expectedCount foreman) $ Just (dispatched, callback)

-- | The blocking version of `complete`
completeAndWait :: Foreman -> IO Int
completeAndWait foreman = do
  mvar <- newEmptyMVar
  complete foreman (putMVar mvar)
  takeMVar mvar