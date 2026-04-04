module Test.Indexer.Reload (tests) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Indexer (indexFile)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (runServerT)
import Server.AgdaProjectResolver (findAgdaProject)
import Server.Model.Monad (runWithAgdaProjectT)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified TestData

tests :: TestTree
tests =
  testGroup "Reload" $
    [ testCase "Reload single file" $ testReloadFile "test/data/A.agda",
      testCase "Reload after changes" testReloadChanges
    ]

testReloadFile :: FilePath -> IO ()
testReloadFile path = do
  let uri = LSP.filePathToUri path
  model <- TestData.getModel

  LSP.runLspT undefined $ do
    env <- TestData.getServerEnv model
    runServerT env $ do
      project <- findAgdaProject uri
      runWithAgdaProjectT project $ do
        src <- TestData.parseSourceFromPath path
        _ <- indexFile src
        _ <- indexFile src
        return ()

testReloadChanges :: IO ()
testReloadChanges = do
  let path = "test/data/A.agda"
  let contentsA =
        Text.unlines
          [ "module A where",
            "",
            "data T : Set where",
            "  tt : T"
          ]
  let contentsB =
        Text.unlines
          [ "module A where",
            "",
            "data T : Set where",
            "  tt : T",
            "",
            "f : T -> T",
            "f x = x"
          ]

  let uri = LSP.filePathToUri path
  model <- TestData.getModel

  LSP.runLspT undefined $ do
    env <- TestData.getServerEnv model
    runServerT env $ do
      project <- findAgdaProject uri
      runWithAgdaProjectT project $ do
        src0 <- TestData.parseSourceFromPathAndContents path contentsA
        agdaFile0 <- indexFile src0

        src1 <- TestData.parseSourceFromPathAndContents path contentsB
        agdaFile1 <- indexFile src1

        src2 <- TestData.parseSourceFromPathAndContents path contentsA
        agdaFile2 <- indexFile src2

        liftIO $ agdaFile0 @?= agdaFile2
