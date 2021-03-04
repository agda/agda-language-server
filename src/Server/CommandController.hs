module Server.CommandController
  ( CommandController,
    new,
    take,
    release,
    put,
  )
where

import Agda.Interaction.Base (IOTCM)
import Control.Concurrent
import Control.Concurrent.SizedChan
import Control.Monad (forM_)
import Prelude hiding (take)

data CommandController
  = CommandController
      (SizedChan IOTCM)
      -- ^ Unbounded Command queue
      (MVar IOTCM)
      -- ^ MVar for the Command consumer

new :: IO CommandController
new = CommandController <$> newSizedChan <*> newEmptyMVar

-- | Blocks if the front is empty
take :: CommandController -> IO IOTCM
take (CommandController _ front) = takeMVar front

-- | Move the payload from the queue to the front
-- Does not block if the front or the queue is empty
release :: CommandController -> IO ()
release (CommandController queue front) = do
  result <- tryReadSizedChan queue
  forM_ result (tryPutMVar front)

-- | Does not block
-- Move the payload to the front if the front is empty
put :: CommandController -> IOTCM -> IO ()
put (CommandController queue front) command = do
  isEmpty <- isEmptyMVar front
  if isEmpty
    then putMVar front command
    else writeSizedChan queue command