{-# LANGUAGE OverloadedStrings #-}

module Test.LSP (tests) where

import Agda
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
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

    -- hover
    TResponseMessage _ _ rsp <- request SMethod_TextDocumentHover (HoverParams doc (Position 3 9) Nothing)
    case rsp of
      Right (InL (Hover (InL (MarkupContent _ content)) (Just (Range start end)))) -> liftIO $ do
        -- disregard the content of the hover message for now
        -- because it varies depending on the version of Agda
        -- content @?= "\n```agda-language-server\nAgda.Primitive.Set\n```\n"
        start @?= Position 3 9
        end @?= Position 3 9
      _ -> liftIO $ assertFailure "Unexpected response"

    -- agda-mode:load
    testCustomMethod "IOTCM \"test/data/A.agdaa\" NonInteractive Direct( Cmd_load \"test/data/A.agda\" [] )"

-- | Sends a custom method request to the server and expects a response of `CmdRes Nothing`
testCustomMethod :: String -> Session ()
testCustomMethod cmd = do
  TResponseMessage _ _ rsp <-
    request agdaCustomMethod $
      JSON.toJSON $
        CmdReq cmd
  liftIO $ rsp @?= Right (JSON.toJSON (CmdRes Nothing))