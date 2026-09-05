{-# LANGUAGE CPP #-}
module Reactor where

import GHC.Wasm.Prim
import Options
import Server (serverDefn)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)

import Data.ByteString.Lazy (LazyByteString)
import Language.LSP.Server (ServerConfig(..), runServerWithConfig)
import qualified Data.Attoparsec.ByteString as Attoparsec

data ReactorEnv = ReactorEnv
  { options :: Options
  , incomingMessage :: MVar B.StrictByteString
  , outgoingMessage :: MVar String
  }

initialEnv :: IO ReactorEnv
initialEnv = ReactorEnv <$> getOptionsFromArgv <*> newEmptyMVar <*> newEmptyMVar

type ServerHandle = StablePtr ReactorEnv

foreign export javascript "run_setup"
  runSetup :: IO ()

foreign export javascript "new_language_server"
  newLanguageServer :: IO ServerHandle

foreign export javascript "run_language_server"
  runLanguageServer :: ServerHandle -> IO Int

foreign export javascript "free_language_server"
  freeLanguageServer :: ServerHandle -> IO ()

foreign export javascript "send_message"
  sendMessage :: ServerHandle -> JSString -> IO ()

foreign export javascript "recv_message"
  recvMessage :: ServerHandle -> IO JSString

runSetup :: IO ()
#if MIN_VERSION_Agda(2,8,0)
runSetup = setup True
#else
runSetup = error "This Agda version does not have setup functionality."
#endif

newLanguageServer :: IO ServerHandle
newLanguageServer = initialEnv >>= newStablePtr

freeLanguageServer :: ServerHandle -> IO ()
freeLanguageServer = freeStablePtr

runLanguageServer :: ServerHandle -> IO Int
runLanguageServer hdl = do
  env <- deRefStablePtr hdl

  let
    serverInwards :: IO B.StrictByteString
    serverInwards = takeMVar (incomingMessage env)

    serverOutwards :: BL.LazyByteString -> IO ()
    serverOutwards s = (return . TL.unpack . decodeUtf8) s >>= putMVar (outgoingMessage env)

  runFromReactor serverInwards serverOutwards (options env)

runFromReactor :: IO B.StrictByteString -> (LazyByteString -> IO ()) -> Options -> IO Int
runFromReactor serverInwards serverOutwards options = do
  runServerWithConfig serverConfig (serverDefn options)
  where
    serverConfig :: ServerConfig Config
    serverConfig = ServerConfig
      { ioLogger = mempty
      , lspLogger = mempty
      , inwards = serverInwards
      , outwards = serverOutwards
      , prepareOutwards = id
      , parseInwards = do
          -- using takeByteString here will make it return partial result,
          -- requiring another empty string to signal its end
          chunk <- Attoparsec.getChunk
          case chunk of
            Nothing -> pure B.empty
            Just xs -> Attoparsec.take $ B.length xs
      }

sendMessage :: ServerHandle -> JSString -> IO ()
sendMessage hdl s = do
  env <- deRefStablePtr hdl
  let input = fromJSString s
  putMVar (incomingMessage env) $ (encodeUtf8 . T.pack) input
  return ()

recvMessage :: ServerHandle -> IO JSString
recvMessage hdl = do
  env <- deRefStablePtr hdl
  str <- takeMVar (outgoingMessage env)
  return $ toJSString str

-- for shimming types when GHC.Wasm.Prim is not available, e.g., when using HLS
#if 0
data JSVal = JSVal {}
newtype JSString = JSString JSVal

fromJSString :: JSString -> String
fromJSString = undefined
toJSString :: String -> JSString
toJSString = undefined
#endif
