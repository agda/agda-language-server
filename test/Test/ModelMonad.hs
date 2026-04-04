{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Test.ModelMonad (tests) where

import Agda.Utils.Either (isLeft, isRight)
import Agda.Utils.IORef (newIORef, readIORef, writeIORef)
import Agda.Utils.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Utils.SMethodMap as SMethodMap
import qualified Language.LSP.Server as LSP
import Monad (ServerT, runServerT)
import Options (Config)
import Server.Model (Model)
import Server.Model.AgdaLib (agdaLibIncludes)
import Server.Model.Handler (requestHandlerWithAgdaFile)
import Server.Model.Monad (askAgdaLib)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import qualified TestData

tests :: TestTree
tests =
  testGroup
    "Model monads"
    [ testGroup
        "WithAgdaFileM"
        [ --  testCase "gets known Agda file" $ do
          --   let method = LSP.SMethod_TextDocumentDocumentSymbol
          --       message = TestData.documentSymbolMessage TestData.fileUri1

          --   let handlers = requestHandlerWithAgdaFile method $ \req responder -> do
          --         agdaLib <- askAgdaLib
          --         liftIO $ length (agdaLib ^. agdaLibIncludes) @?= 3
          --         responder $ Right $ LSP.InL []

          --   model <- TestData.getModel
          --   result <- runHandler method message model handlers

          --   isRight result @? "didn't get known Agda file: " <> show result

          --   return (),
          testCase "fails on unknown Agda file" $ do
            let method = LSP.SMethod_TextDocumentDocumentSymbol
                message = TestData.documentSymbolMessage TestData.fakeUri

            let handlers = requestHandlerWithAgdaFile method $ \req responder -> do
                  responder $ Right $ LSP.InL []

            model <- TestData.getModel
            result <- runHandler method message model handlers

            isLeft result @? "got Agda file, but should be unknown"

            return ()
        ]
    ]

runHandler ::
  forall (m :: LSP.Method LSP.ClientToServer LSP.Request).
  (Show (LSP.ErrorData m)) =>
  LSP.SMethod m ->
  LSP.TRequestMessage m ->
  Model ->
  LSP.Handlers (ServerT (LSP.LspM Config)) ->
  IO (Either (LSP.TResponseError m) (LSP.MessageResult m))
runHandler m request model handlers = do
  resultRef <- newIORef Nothing
  let callback = \response -> liftIO $ writeIORef resultRef (Just response)

  let Just (LSP.ClientMessageHandler handler) = SMethodMap.lookup m $ LSP.reqHandlers handlers

  LSP.runLspT undefined $ do
    env <- TestData.getServerEnv model
    runServerT env $ handler request callback

  Just result <- readIORef resultRef
  return result
