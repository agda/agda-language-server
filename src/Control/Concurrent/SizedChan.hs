-- | Chan with size
module Control.Concurrent.SizedChan (SizedChan, newSizedChan, writeSizedChan, readSizedChan, tryReadSizedChan, peekSizedChan, tryPeekSizedChan, isEmptySizedChan) where

import Control.Concurrent.Chan
import Data.IORef

data SizedChan a = 
  SizedChan 
    (Chan a) -- ^ The channel
    (IORef Int) -- ^ Its size
    (IORef (Maybe a)) -- ^ Peeked payload

-- | Build and returns a new instance of 'SizedChan'.
newSizedChan :: IO (SizedChan a)
newSizedChan =
  SizedChan
    <$> newChan
    <*> newIORef 0
    <*> newIORef Nothing

-- | Write a value to a 'SizedChan'.
writeSizedChan :: SizedChan a -> a -> IO ()
writeSizedChan (SizedChan chan sizeIORef _) val = do
  writeChan chan val
  modifyIORef' sizeIORef succ

-- | Read the next value from the 'SizedChan'. Blocks when the channel is empty.
readSizedChan :: SizedChan a -> IO a
readSizedChan (SizedChan chan sizeIORef peekedIORef) = do
  peeked <- readIORef peekedIORef
  case peeked of
    -- return and remove the peeked value
    Just val -> do
      writeIORef peekedIORef Nothing
      modifyIORef' sizeIORef pred
      return val
    -- else read from the channel
    Nothing -> do
      val <- readChan chan
      modifyIORef' sizeIORef pred
      return val

-- | A version of `readSizedChan` which does not block. Instead it returns Nothing if no value is available.
tryReadSizedChan :: SizedChan a -> IO (Maybe a)
tryReadSizedChan (SizedChan chan sizeIORef peekedIORef) = do
  peeked <- readIORef peekedIORef
  case peeked of
    -- return and remove the peeked value
    Just val -> do
      writeIORef peekedIORef Nothing
      modifyIORef' sizeIORef pred
      return $ Just val
    -- check the size before reading from the channel, to prevent blocking
    Nothing -> do
      size <- readIORef sizeIORef
      if size == 0
        then return Nothing
        else do
          val <- readChan chan
          modifyIORef' sizeIORef pred
          return $ Just val

-- | Peek the next value from the 'SizedChan' without removing it. Blocks when the channel is empty.
peekSizedChan :: SizedChan a -> IO a
peekSizedChan (SizedChan chan _ peekedIORef) = do
  peeked <- readIORef peekedIORef
  case peeked of
    -- return the peeked value
    Just val -> return val
    -- read from the channel instead
    Nothing -> do 
      val <- readChan chan
      writeIORef peekedIORef (Just val)
      return val

-- | A version of `peekSizedChan` which does not block. Instead it returns Nothing if no value is available.
tryPeekSizedChan :: SizedChan a -> IO (Maybe a)
tryPeekSizedChan (SizedChan chan sizeIORef peekedIORef) = do 
  peeked <- readIORef peekedIORef
  case peeked of
    -- return the peeked value
    Just val -> return $ Just val
    -- check the size before reading from the channel, to prevent blocking
    Nothing -> do
      size <- readIORef sizeIORef
      if size == 0
        then return Nothing
        else do
          val <- readChan chan
          writeIORef peekedIORef (Just val)
          return $ Just val

measureSizedChan :: SizedChan a -> IO Int
measureSizedChan (SizedChan _ sizeIORef _) = readIORef sizeIORef

isEmptySizedChan :: SizedChan a -> IO Bool
isEmptySizedChan chan = do
  size <- measureSizedChan chan
  return $ size == 0
