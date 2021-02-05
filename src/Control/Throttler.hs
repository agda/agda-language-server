module Control.Throttler
  ( Throttler,
    new,
    take,
    put,
    putWithCallback,
  )
where

import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import Prelude hiding (take)

data Throttler a = Throttler
  { thrQueue :: Chan (a, () -> IO ()),
    thrQueueSize :: IORef Int,
    thrInput :: MVar (a, () -> IO ())
  }

new :: IO (Throttler a)
new = Throttler <$> newChan <*> newIORef 0 <*> newEmptyMVar

-- | For the consumer of the payload
-- | Blocks until the payload is available
-- | Invoke the callback to signal finished handling the payload
take :: Throttler a -> IO (a, () -> IO ())
take throttler = do
  (payload, callback) <- takeMVar (thrInput throttler)
  -- beef-up the callback
  -- so that we can kick start the next cycle once it's invoked
  let callback' () = do
        -- if `thrQueue` is not empty
        -- move the next payload from the queue to `thrInput`
        size <- readIORef (thrQueueSize throttler)
        when (size /= 0) $ do
          next <- readChan (thrQueue throttler)
          modifyIORef' (thrQueueSize throttler) pred
          putMVar (thrInput throttler) next
        callback ()

  return (payload, callback')

-- | For the provider of the payload
-- | Does not block
-- | Callback will be invoked once the payload is handled
putWithCallback :: Throttler a -> a -> (() -> IO ()) -> IO ()
putWithCallback throttler payload callback = do
  -- see if `thrQueue` is empty
  queueSize <- readIORef (thrQueueSize throttler)
  -- see if `thrInput` is empty
  inputEmpty <- isEmptyMVar (thrInput throttler)

  -- putStrLn $ "queueSize: " <> show queueSize <> " inputEmpty: " <> show inputEmpty

  if queueSize == 0 && inputEmpty
    then do
      -- doesn't block since `thrInput` is empty
      putMVar (thrInput throttler) (payload, callback)
    else do
      -- push it into the queue else it will block
      writeChan (thrQueue throttler) (payload, callback)
      modifyIORef' (thrQueueSize throttler) succ

-- | For the provider of the payload
-- | Blocks until the payload is handled
put :: Throttler a -> a -> IO ()
put throttler payload = do
  blocker <- newEmptyMVar
  let callback = putMVar blocker
  putWithCallback throttler payload callback
  takeMVar blocker