module Control.Throttler
  ( Throttler,
    new,
    take,
    put,
    putAndWait,
  )
where

import Control.Concurrent
import Control.Monad (when, forM_)
import Data.IORef
import Prelude hiding (take)
import Control.Concurrent.SizedChan

data Throttler a = Throttler
  { queue :: SizedChan (a, () -> IO ()),
    input :: MVar (a, () -> IO ())
  }

new :: IO (Throttler a)
new = Throttler <$> newSizedChan <*> newEmptyMVar

-- | For the consumer of the payload
-- | Blocks until the payload is available
-- | Invoke the callback to signal finished handling the payload
take :: Throttler a -> IO (a, () -> IO ())
take (Throttler queue input) = do
  (payload, callback) <- takeMVar input
  -- beef-up the callback
  -- so that we can kick start the next cycle once it's invoked
  let callback' () = do
        -- if `thrQueue` is not empty
        -- move the next payload from the queue to `thrInput`
        result <- tryReadSizedChan queue
        forM_ result 
          (putMVar input)
        callback ()

  return (payload, callback')

-- | For the provider of the payload
-- | Does not block
-- | Callback will be invoked once the payload is handled
put :: Throttler a -> a -> (() -> IO ()) -> IO ()
put (Throttler queue input) payload callback = do
  -- see if `thrInput` is empty
  inputEmpty <- isEmptyMVar input

  if inputEmpty
    then putMVar input (payload, callback)
    else writeSizedChan queue (payload, callback)

-- | For the provider of the payload
-- | Blocks until the payload is handled
putAndWait :: Throttler a -> a -> IO ()
putAndWait throttler payload = do
  blocker <- newEmptyMVar
  put throttler payload (putMVar blocker)
  takeMVar blocker