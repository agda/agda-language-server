module Test.Model (tests) where

import Agda.Utils.Lens ((^.))
import Agda.Utils.Maybe (isJust, isNothing)
import qualified Server.Model as Model
import Server.Model.AgdaLib (agdaLibIncludes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import qualified TestData

tests :: TestTree
tests =
  testGroup
    "Model"
    [ --  testCase "getKnownAgdaLib gets known Agda lib" $ do
      --   model <- TestData.getModel

      --   let Just agdaLib = Model.getKnownAgdaLib TestData.fileUri1 model
      --   length (agdaLib ^. agdaLibIncludes) @?= 3

      --   let Just agdaLib = Model.getKnownAgdaLib TestData.fileUri2 model
      --   length (agdaLib ^. agdaLibIncludes) @?= 1

      --   let Just agdaLib = Model.getKnownAgdaLib TestData.fileUri3 model
      --   length (agdaLib ^. agdaLibIncludes) @?= 3

      --   return (),
      -- testCase "getKnownAgdaLib fails on unknown Agda lib" $ do
      --   model <- TestData.getModel

      --   let result = Model.getKnownAgdaLib TestData.fakeUri model
      --   isNothing result @? "got Agda lib, but should be unknown"

      --   return (),
      testCase "getAgdaFile gets known Agda file" $ do
        model <- TestData.getModel

        let result = Model.getAgdaFile TestData.fileUri1 model
        isJust result @? "didn't get known Agda file"

        let result = Model.getAgdaFile TestData.fileUri2 model
        isJust result @? "didn't get known Agda file"

        let result = Model.getAgdaFile TestData.fileUri3 model
        isJust result @? "didn't get known Agda file"

        return (),
      testCase "getAgdaFile fails on unknown Agda file" $ do
        model <- TestData.getModel

        let result = Model.getAgdaFile TestData.fakeUri model
        isNothing result @? "got Agda file, but should be unknown"

        return ()
    ]
