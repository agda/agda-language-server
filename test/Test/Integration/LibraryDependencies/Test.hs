{-# LANGUAGE FlexibleContexts #-}

module Test.Integration.LibraryDependencies.Test (test) where

import Agda.Utils.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.Text (Text)
import Language.LSP.Protocol.Lens (HasName, name)
import Language.LSP.Test
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

test :: FilePath -> TestTree
test alsPath = testCase "Library dependencies" $
  runSession alsPath fullLatestClientCaps "test/Test/Integration/LibraryDependencies/data" $ do
    doc <- openDoc "has-dep/src/Three.agda" "agda"

    notification <- anyNotification
    liftIO $ print notification

    -- Check that we really opened and indexed the file
    docSymbols <- getDocumentSymbols doc

    let fromNamed :: (HasName a Text) => [a] -> [Text]
        fromNamed = fmap $ \named -> named ^. name
    let docSymbolNames = docSymbols & either fromNamed fromNamed

    liftIO $ docSymbolNames @?= ["three"]
