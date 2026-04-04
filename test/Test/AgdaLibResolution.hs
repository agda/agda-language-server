{-# LANGUAGE CPP #-}

module Test.AgdaLibResolution (tests) where

#if MIN_VERSION_Agda(2,8,0)
import Agda.Interaction.Library (parseLibName)
#endif
import Agda.Utils.Lens ((^.))
import Agda.Utils.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Indexer (indexFile)
import qualified Language.LSP.Server as LSP
import Monad (runServerT)
import Server.AgdaProjectResolver (findAgdaProject)
import qualified Server.AgdaProjectResolver as AgdaProjectResolver
import qualified Server.Filesystem as FS
import Server.Model.AgdaLib (agdaLibIncludes, agdaLibName)
import qualified Server.Model.AgdaProject as AgdaProject
import Server.Model.Monad (runWithAgdaProjectT)
import System.Directory (makeAbsolute)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified TestData

natPath, constPath, agdaLibPath, srcPath :: FilePath
natPath = "test/data/libs/no-deps/src/Data/Nat.agda"
constPath = "test/data/libs/no-deps/src/Constants.agda"
agdaLibPath = "test/data/libs/no-deps/no-deps.agda-lib"
srcPath = "test/data/libs/no-deps/src"

tests :: TestTree
tests =
  testGroup
    "Agda lib resolution"
    [ testCase "Explicit" $ do
        model <- TestData.getModel

        absConstPath <- makeAbsolute constPath
        absAgdaLibPath <- makeAbsolute agdaLibPath
        absSrcPath <- makeAbsolute srcPath

        LSP.runLspT undefined $ do
          env <- TestData.getServerEnv model
          runServerT env $ do
            project <- AgdaProjectResolver.findAgdaProject absConstPath
            let lib = project ^. AgdaProject.agdaLib & fromJust
            liftIO $ lib ^. agdaLibName @?= parseLibName "no-deps"
            liftIO $ lib ^. agdaLibIncludes @?= [FS.LocalFilePath absSrcPath],
      testCase "Module without imports in lib without dependencies" $ do
        model <- TestData.getModel

        LSP.runLspT undefined $ do
          env <- TestData.getServerEnv model
          runServerT env $ do
            natProject <- findAgdaProject natPath
            runWithAgdaProjectT natProject $ do
              natSrc <- TestData.parseSourceFromPath natPath
              _ <- indexFile natSrc
              return (),
      testCase "Module with imports in lib without lib dependencies" $ do
        model <- TestData.getModel

        LSP.runLspT undefined $ do
          env <- TestData.getServerEnv model
          runServerT env $ do
            constProject <- findAgdaProject constPath
            runWithAgdaProjectT constProject $ do
              constSrc <- TestData.parseSourceFromPath constPath
              _ <- indexFile constSrc
              return ()
    ]
