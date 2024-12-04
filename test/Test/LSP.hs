{-# LANGUAGE OverloadedStrings #-}

module Test.LSP (tests) where

import Agda
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Language.LSP.Protocol.Message
import Language.LSP.Test
import Switchboard (agdaCustomMethod)
import Test.Tasty
import Test.Tasty.HUnit

tests :: FilePath -> TestTree
tests alsPath =
  testGroup
    "LSP"
    [ testCase "load" (demo alsPath)
    ]

demo :: FilePath -> IO ()
demo alsPath = do
  putStrLn $ "Running LSP tests on the server with the following path to the als executable: " ++ alsPath
  runSession alsPath fullLatestClientCaps "test/data/" $ do
    doc <- openDoc "A.agda" "agda"

    -- Use your favourite favourite combinators.
    -- skipManyTill loggingNotification (count 1 publishDiagnosticsNotification)

    testCustomMethod "IOTCM \"test/data/A.agdaa\" NonInteractive Direct( Cmd_load \"test/data/A.agda\" [] )"

-- Or use one of the helper functions
-- getDocumentSymbols doc >>= liftIO . print

-- | Sends a custom method request to the server and expects a response of `CmdRes Nothing`
testCustomMethod :: String -> Session ()
testCustomMethod cmd = do
  TResponseMessage _ _ rsp <-
    request agdaCustomMethod $
      JSON.toJSON $
        CmdReq cmd
  liftIO $ rsp @?= Right (JSON.toJSON (CmdRes Nothing))