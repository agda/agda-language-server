module Switchboard (run) where


import qualified Core
import Common
import qualified Data.Text.IO as Text
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Reader

-- | All channels go in and out from here
run :: Env -> IO ()
run env = do 
  forkIO (keepPrintingLog env)

  forkIO $ runReaderT Core.interact env
  return ()

-- | Keep printing log 
-- Consumer of `envLogChan`
keepPrintingLog :: Env -> IO ()
keepPrintingLog env = do
  result <- readChan (envLogChan env)
  when (envDevMode env) $ do
    Text.putStrLn result
  keepPrintingLog env