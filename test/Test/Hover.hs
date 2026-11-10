{-# LANGUAGE OverloadedStrings #-}

-- | Hover tests (#68)
module Test.Hover (tests) where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess (cwd, env))
import Test.LSP (withTempDir)
import Test.Tasty
import Test.Tasty.HUnit

tests :: FilePath -> TestTree
tests alsPath =
  testGroup
    "Hover"
    [ testCase "hover the same with or without a valid .agdai" (hoverWithInterface alsPath)
    ]

-- | The first hover checks Main.agda and Dep.agda and writes their .agdai
-- files. A new server then hovers at the same place, with those .agdai
-- files valid. The hover text must not depend on whether a .agdai was
-- there.
hoverWithInterface :: FilePath -> IO ()
hoverWithInterface alsPath = withFixtures alsPath "Hover" "als-hover-interface" $ \dir -> do
  doc <- openDoc "Main.agda" "agda"
  expectHover doc onF "T → T"
  liftIO $ runSessionIn alsPath dir $ do
    fresh <- openDoc (docPath doc) "agda"
    expectHover fresh onF "T → T"

--------------------------------------------------------------------------------
-- Helpers

-- | Copies the fixtures in test/data/<fixtures> to a temporary directory
-- and runs a session there
withFixtures :: FilePath -> FilePath -> String -> (FilePath -> Session a) -> IO a
withFixtures alsPath fixtures name session =
  withTempDir name $ \dir -> do
    copyTree ("test" </> "data" </> fixtures) dir
    runSessionIn alsPath dir (session dir)

-- | Runs a session in the given directory, with an empty AGDA_DIR there,
-- so the libraries and defaults of the user running the tests stay out
runSessionIn :: FilePath -> FilePath -> Session a -> IO a
runSessionIn alsPath dir session = do
  let agdaDir = dir </> ".agda"
  createDirectoryIfMissing True agdaDir
  environment <- filter ((/= "AGDA_DIR") . fst) <$> getEnvironment
  let inDir process = process {cwd = Just dir, env = Just (("AGDA_DIR", agdaDir) : environment)}
  runSessionWithConfigCustomProcess inDir defaultConfig alsPath fullLatestClientCaps dir session

-- | Copies a directory and everything in it
copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  createDirectoryIfMissing True to
  entries <- listDirectory from
  forM_ entries $ \entry -> do
    isDirectory <- doesDirectoryExist (from </> entry)
    if isDirectory
      then copyTree (from </> entry) (to </> entry)
      else copyFile (from </> entry) (to </> entry)

-- | Where 'f' is in 'g = f' of Main.agda
onF :: Position
onF = Position 5 4

docPath :: TextDocumentIdentifier -> FilePath
docPath (TextDocumentIdentifier docUri) = case uriToFilePath docUri of
  Just path -> path
  Nothing -> error ("not a file: " ++ show docUri)

-- | Hovers at a position and returns the text of the hover
hoverText :: TextDocumentIdentifier -> Position -> Session Text
hoverText doc pos = do
  TResponseMessage _ _ rsp <- request SMethod_TextDocumentHover (HoverParams doc pos Nothing)
  case rsp of
    Right (InL (Hover (InL (MarkupContent _ content)) _)) -> return content
    _ -> liftIO $ assertFailure ("Unexpected hover response: " ++ show rsp)

-- | Expects the hover to show the given type
expectHover :: HasCallStack => TextDocumentIdentifier -> Position -> Text -> Session ()
expectHover doc pos typ = do
  actual <- hoverText doc pos
  liftIO $ actual @?= "\n```agda-language-server\n" <> typ <> "\n```\n"
