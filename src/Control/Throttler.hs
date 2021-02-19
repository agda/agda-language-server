module Control.Throttler
  ( Throttler,
    new,
    take,
    move,
    put,
  )
where

import Control.Concurrent
import Control.Monad (when, forM_)
import Data.IORef
import Prelude hiding (take)
import Control.Concurrent.SizedChan

data Throttler a = Throttler
  { queue :: SizedChan a,
    front :: MVar a
  }

new :: IO (Throttler a)
new = Throttler <$> newSizedChan <*> newEmptyMVar

-- | Blocks if the front is empty
take :: Throttler a -> IO a
take (Throttler _ front) = takeMVar front

-- | Move the payload from the queue to the front 
-- Does not block if the front or the queue is empty
move :: Throttler a -> IO ()
move (Throttler queue front) = do 
  result <- tryReadSizedChan queue
  forM_ result (tryPutMVar front)

-- | Does not block
-- Move the payload to the front if the front is empty
put :: Throttler a -> a -> IO ()
put (Throttler queue front) payload = do
  isEmpty <- isEmptyMVar front
  if isEmpty
    then putMVar front payload
    else writeSizedChan queue payload