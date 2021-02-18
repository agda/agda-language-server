-- | Chan with size
module Control.Concurrent.SizedChan where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)


data SizedChan a = SizedChan
  { -- | The channel
    chan :: Chan a,
    -- | Its size
    size :: IORef Int
  }

-- | Build and returns a new instance of 'SizedChan'.
newSizedChan :: IO (SizedChan a)
newSizedChan =
  SizedChan
    <$> newChan
    <*> newIORef 0

-- | Write a value to a 'SizedChan'.
writeSizedChan :: SizedChan a -> a -> IO ()
writeSizedChan (SizedChan chan sizeIORef) val = do
  writeChan chan val
  modifyIORef' sizeIORef succ

-- | Read the next value from the 'SizedChan'. Blocks when the channel is empty.
readSizedChan :: SizedChan a -> IO a
readSizedChan (SizedChan chan sizeIORef) = do
  val <- readChan chan
  modifyIORef' sizeIORef pred
  return val

-- -- | Peek the next value from the 'SizedChan' without removing it. Blocks when the channel is empty.
-- peekSizedChan :: SizedChan a -> IO a
-- peekSizedChan (SizedChan chan sizeIORef) = do
--   val <- tryReadSizedChan chan
--   modifyIORef' sizeIORef pred
--   return val

measureSizedChan :: SizedChan a -> IO Int 
measureSizedChan (SizedChan _ sizeIORef) = readIORef sizeIORef

isEmptySizedChan :: SizedChan a -> IO Bool
isEmptySizedChan chan = do 
  size <- measureSizedChan chan
  return $ size == 0

tryReadSizedChan :: SizedChan a -> IO (Maybe a)
tryReadSizedChan chan = do 
  isEmpty <- isEmptySizedChan chan 
  if isEmpty 
    then return Nothing 
    else Just <$> readSizedChan chan