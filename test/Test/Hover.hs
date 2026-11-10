{-# LANGUAGE OverloadedStrings #-}

-- | Hover tests (#68)
module Test.Hover (tests) where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.Stack (HasCallStack)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getModificationTime, listDirectory, removeFile, setModificationTime)
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
    [ hoverAfterEdits alsPath,
      testCase "hover the same with or without a valid .agdai" (hoverWithInterface alsPath)
    ]

-- | Each test hovers, changes files on disk the way an editor or a git
-- checkout does, and hovers again in the same session. The hover must
-- show what a fresh load of the files on disk shows. Types are pinned as
-- text. Errors are compared with the hover of a fresh server, because
-- their text holds the temporary path.
hoverAfterEdits :: FilePath -> TestTree
hoverAfterEdits alsPath =
  testGroup
    "hover after edits"
    [ testCase "edit the hovered file" $ withFixtures alsPath "Hover" "als-hover-file" $ \_ -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        save doc "module Main where\n\nopen import Dep hiding (f)\n\nf : T -> T -> T\nf x _ = x\n\ng : _\ng = f\n"
        expectHover doc (Position 8 4) "T → T → T",
      testCase "edit an import, then undo the edit" $ withFixtures alsPath "Hover" "als-hover-import" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        original <- readText (dir </> "Dep.agda")
        writeText (dir </> "Dep.agda") (addArgumentToF original)
        expectHover doc onF "T → T → T"
        writeText (dir </> "Dep.agda") original
        expectHover doc onF "T → T",
      testCase "edit an import of an import" $ withFixtures alsPath "Hover" "als-hover-transitive" $ \dir -> do
        doc <- openDoc "ViaMid.agda" "agda"
        expectHover doc onF "T → T"
        original <- readText (dir </> "Dep.agda")
        writeText (dir </> "Dep.agda") (addArgumentToF original)
        expectHover doc onF "T → T → T",
      testCase "save with no change" $ withFixtures alsPath "Hover" "als-hover-touch" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        forM_ ["Main.agda", "Dep.agda"] $ \file -> readText (dir </> file) >>= writeText (dir </> file)
        expectHover doc onF "T → T",
      testCase "a new file shadows an import" $ withFixtures alsPath "HoverShadow" "als-hover-shadow" $ \dir -> do
        doc <- openDoc ("main" </> "Main.agda") "agda"
        expectHover doc onF "T → T"
        liftIO $ copyFile (dir </> "dep" </> "Dep.agda") (dir </> "main" </> "Dep.agda")
        expectFreshHover alsPath dir doc onF,
      testCase "delete an import, then restore it" $ withFixtures alsPath "Hover" "als-hover-delete" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        original <- readText (dir </> "Dep.agda")
        liftIO $ removeFile (dir </> "Dep.agda")
        expectFreshHover alsPath dir doc onF
        writeText (dir </> "Dep.agda") original
        expectHover doc onF "T → T",
      testCase "rename an import" $ withFixtures alsPath "Hover" "als-hover-rename" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        dep <- readText (dir </> "Dep.agda")
        liftIO $ removeFile (dir </> "Dep.agda")
        writeText (dir </> "Renamed.agda") (Text.replace "module Dep" "module Renamed" dep)
        expectFreshHover alsPath dir doc onF
        main <- readText (dir </> "Main.agda")
        save doc (Text.replace "import Dep" "import Renamed" main)
        expectHover doc onF "T → T",
      testCase "a literate file replaces an import" $ withFixtures alsPath "Hover" "als-hover-literate" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        dep <- readText (dir </> "Dep.agda")
        liftIO $ removeFile (dir </> "Dep.agda")
        writeText (dir </> "Dep.lagda.md") ("```agda\n" <> addArgumentToF dep <> "```\n")
        expectHover doc onF "T → T → T",
      testCase "a parse error in the hovered file, then fixed" $ withFixtures alsPath "Hover" "als-hover-parse" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        main <- readText (dir </> "Main.agda")
        save doc (main <> "\nh = (\n")
        expectFreshHover alsPath dir doc onF
        save doc main
        expectHover doc onF "T → T",
      testCase "a hole in the hovered file, then an import edit" $ withFixtures alsPath "Hover" "als-hover-hole" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        main <- readText (dir </> "Main.agda")
        save doc (main <> "\nh : T\nh = {! !}\n")
        expectHover doc onF "T → T"
        dep <- readText (dir </> "Dep.agda")
        writeText (dir </> "Dep.agda") (addArgumentToF dep)
        expectHover doc onF "T → T → T"
        save doc (main <> "\nh : T\nh = t\n")
        expectHover doc onF "T → T → T",
      testCase "an import cycle, then fixed" $ withFixtures alsPath "Hover" "als-hover-cycle" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        dep <- readText (dir </> "Dep.agda")
        writeText (dir </> "Dep.agda") ("open import Main\n" <> dep)
        expectFreshHover alsPath dir doc onF
        writeText (dir </> "Dep.agda") dep
        expectHover doc onF "T → T",
      testCase "switch branches and back, which restores old timestamps" $ withFixtures alsPath "Hover" "als-hover-branch" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        let file = dir </> "Dep.agda"
        dep <- readText file
        time <- liftIO $ getModificationTime file
        writeText file (addArgumentToF dep)
        expectHover doc onF "T → T → T"
        writeText file dep
        liftIO $ setModificationTime file time
        expectHover doc onF "T → T"
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

-- | Where 'f' is in 'g = f' of Main.agda and ViaMid.agda
onF :: Position
onF = Position 5 4

-- | Changes 'f : T -> T' in Dep.agda to take two arguments
addArgumentToF :: Text -> Text
addArgumentToF = Text.replace "f : T -> T\nf x = x" "f : T -> T -> T\nf x _ = x"

readText :: FilePath -> Session Text
readText = liftIO . TextIO.readFile

writeText :: FilePath -> Text -> Session ()
writeText file = liftIO . TextIO.writeFile file

-- | Saves an open file the way an editor does: the new text goes to the
-- server and to the disk
save :: TextDocumentIdentifier -> Text -> Session ()
save doc text = do
  changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument text))]
  writeText (docPath doc) text

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

-- | Expects the hover to be the same as that of a fresh server
expectFreshHover :: HasCallStack => FilePath -> FilePath -> TextDocumentIdentifier -> Position -> Session ()
expectFreshHover alsPath dir doc pos = do
  actual <- hoverText doc pos
  expected <- liftIO $ runSessionIn alsPath dir $ do
    fresh <- openDoc (docPath doc) "agda"
    hoverText fresh pos
  liftIO $ actual @?= expected
