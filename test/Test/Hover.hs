{-# LANGUAGE OverloadedStrings #-}

-- | Hover tests (#68)
module Test.Hover (tests) where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.Stack (HasCallStack)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getModificationTime, getPermissions, listDirectory, removeFile, setModificationTime, setOwnerExecutable, setPermissions)
import System.Environment (getEnvironment)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.Info (os)
import System.Process (CreateProcess (cwd, env))
import Test.LSP (runIOTCM, withTempDir)
import Test.Tasty
import Test.Tasty.HUnit

tests :: FilePath -> TestTree
tests alsPath =
  testGroup
    "Hover"
    [ hoverAfterEdits alsPath,
      hoverAfterImportGraphChanges alsPath,
      hoverAfterProjectChanges alsPath,
      hoverInSession alsPath,
      testCase "hover the same with or without a valid .agdai" (hoverWithInterface alsPath),
      testCase "with --no-hover-cache, every hover loads again" (hoverWithoutCache alsPath)
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

-- | Like 'hoverAfterEdits', but an import breaks deeper in the import
-- graph: ViaMid imports Mid, which imports Dep. Agda reports the error
-- where the broken import is, and checks the imports in the order of the
-- declarations. A reload must give the same error as a fresh server.
hoverAfterImportGraphChanges :: FilePath -> TestTree
hoverAfterImportGraphChanges alsPath =
  testGroup "hover after an import breaks deep in the import graph" $
    [ testCase (shape ++ ", " ++ change ++ " Dep") $ withFixtures alsPath "Hover" ("als-hover-graph-" ++ show i) $ \dir -> do
        writeText (dir </> "ViaMid.agda") ("module ViaMid where\n\n" <> imports <> "\ng : _\ng = f\n")
        -- a second include directory, empty until a file shadows Dep
        writeText (dir </> "graph.agda-lib") "name: graph\ninclude: . extra\n"
        liftIO $ createDirectoryIfMissing True (dir </> "extra")
        doc <- openDoc "ViaMid.agda" "agda"
        let onG = Position (fromIntegral (4 + length (Text.lines imports))) 4
        expectHover doc onG "T → T"
        dep <- readText (dir </> "Dep.agda")
        case change of
          "delete" -> liftIO $ removeFile (dir </> "Dep.agda")
          "rename" -> do
            liftIO $ removeFile (dir </> "Dep.agda")
            writeText (dir </> "Renamed.agda") (Text.replace "module Dep" "module Renamed" dep)
          _ -> writeText (dir </> "extra" </> "Dep.agda") dep
        expectFreshHover alsPath dir doc onG
    | (i, (shape, imports, change)) <-
        zip [1 :: Int ..]
          [ (shape, imports, change)
          | (shape, imports) <-
              [ ("ViaMid imports Mid", "open import Mid\n"),
                ("ViaMid imports Dep, then Mid", "open import Dep\nopen import Mid\n"),
                ("ViaMid imports Mid, then Dep", "open import Mid\nopen import Dep\n")
              ],
            change <- ["delete", "rename", "shadow"]
          ]
    ]
      ++ [ testCase ("Dep keeps its path but " ++ fault) $ withFixtures alsPath "Hover" ("als-hover-graph-fault-" ++ show i) $ \dir -> do
             doc <- openDoc "ViaMid.agda" "agda"
             expectHover doc onF "T → T"
             dep <- readText (dir </> "Dep.agda")
             writeText (dir </> "Dep.agda") (breakDep dep)
             expectFreshHover alsPath dir doc onF
         | (i, (fault, breakDep)) <-
             zip
               [1 :: Int ..]
               [ ("names another module", Text.replace "module Dep" "module Other"),
                 ("has an unknown option", ("{-# OPTIONS --no-such-option #-}\n" <>)),
                 ("has a type error", Text.replace "f x = x" "f x = Set"),
                 ("has a postulate under --safe", \dep -> "{-# OPTIONS --safe #-}\n" <> dep <> "\npostulate bad : T\n"),
                 ("imports a missing module", Text.replace "module Dep where\n" "module Dep where\n\nopen import Missing\n"),
                 ("has a parse error", (<> "\nh = (\n"))
               ]
         ]
      ++ [ testCase ("two imports break, " ++ first ++ " imported first") $ withFixtures alsPath "Hover" ("als-hover-graph-two-" ++ first) $ \dir -> do
             writeText (dir </> "A.agda") "module A where\nopen import MissingA public\n"
             writeText (dir </> "Z.agda") "module Z where\nopen import MissingZ public\n"
             writeText (dir </> "MissingA.agda") "module MissingA where\npostulate TA : Set\n"
             writeText (dir </> "MissingZ.agda") "module MissingZ where\npostulate TZ : Set\n"
             let imports = if first == "Z" then "import Z\nimport A\n" else "import A\nimport Z\n"
             writeText (dir </> "Main.agda") ("module Main where\n\n" <> imports <> "open import Dep\ng : _\ng = f\n")
             doc <- openDoc "Main.agda" "agda"
             expectHover doc (Position 6 4) "T → T"
             liftIO $ removeFile (dir </> "MissingA.agda")
             liftIO $ removeFile (dir </> "MissingZ.agda")
             expectFreshHover alsPath dir doc (Position 6 4)
         | first <- ["A", "Z"]
         ]
      ++ [ testCase "a module in a subdirectory gets a shadow, then moves" $ withFixtures alsPath "Hover" "als-hover-graph-subdirectory" $ \dir -> do
             let dep = "module Sub.Dep where\n\ndata T : Set where\n  t : T\n\nf : T -> T\nf x = x\n"
             writeText (dir </> "sub.agda-lib") "name: sub\ninclude: src extra\n"
             liftIO $ createDirectoryIfMissing True (dir </> "src" </> "Sub")
             liftIO $ createDirectoryIfMissing True (dir </> "extra")
             writeText (dir </> "src" </> "Sub" </> "Dep.agda") dep
             writeText (dir </> "src" </> "UsesSub.agda") "module UsesSub where\n\nopen import Sub.Dep\n\ng : _\ng = f\n"
             doc <- openDoc ("src" </> "UsesSub.agda") "agda"
             expectHover doc onF "T → T"
             -- extra/Sub did not exist before
             liftIO $ createDirectoryIfMissing True (dir </> "extra" </> "Sub")
             writeText (dir </> "extra" </> "Sub" </> "Dep.agda") dep
             expectFreshHover alsPath dir doc onF
             liftIO $ removeFile (dir </> "src" </> "Sub" </> "Dep.agda")
             expectHover doc onF "T → T",
           -- the edit after the move tells whether the hover reads Dep from
           -- its new place
           testCase "Dep moves to another listed include dir, then changes" $ withFixtures alsPath "Hover" "als-hover-graph-move" $ \dir -> do
             writeText (dir </> "graph.agda-lib") "name: graph\ninclude: . extra\n"
             liftIO $ createDirectoryIfMissing True (dir </> "extra")
             doc <- openDoc "ViaMid.agda" "agda"
             expectHover doc onF "T → T"
             dep <- readText (dir </> "Dep.agda")
             liftIO $ removeFile (dir </> "Dep.agda")
             writeText (dir </> "extra" </> "Dep.agda") dep
             expectHover doc onF "T → T"
             writeText (dir </> "extra" </> "Dep.agda") (addArgumentToF dep)
             expectHover doc onF "T → T → T"
         ]

-- | Like 'hoverAfterEdits', but the changes are to the project setup:
-- the .agda-lib files, the libraries file and the trusted executables
hoverAfterProjectChanges :: FilePath -> TestTree
hoverAfterProjectChanges alsPath =
  testGroup "hover after project changes" $
    [ testCase "add an include directory that has the import" $ withFixtures alsPath "HoverLibrary" "als-hover-include" $ \dir -> do
        doc <- openDoc ("src" </> "Main.agda") "agda"
        expectHover doc onF "T → T"
        liftIO $ removeFile (dir </> "src" </> "Dep.agda")
        writeText (dir </> "hover-library.agda-lib") "name: hover-library\ninclude: src other\n"
        expectHover doc onF "T → T → T",
      -- no .agda file changes here, so a change in the include dirs is the
      -- only thing that tells the hover to reload
      testCase "only the include dirs change, making an import ambiguous" $ withFixtures alsPath "HoverLibrary" "als-hover-only-includes" $ \dir -> do
        doc <- openDoc ("src" </> "Main.agda") "agda"
        expectHover doc onF "T → T"
        writeText (dir </> "hover-library.agda-lib") "name: hover-library\ninclude: src other\n"
        expectFreshHover alsPath dir doc onF,
      testCase "remove a flag the hovered file needs" $ withFixtures alsPath "HoverLibrary" "als-hover-flags" $ \dir -> do
        let library = dir </> "hover-library.agda-lib"
        original <- readText library
        writeText library (original <> "flags: --prop\n")
        doc <- openDoc ("src" </> "UsesProp.agda") "agda"
        expectHover doc onF "T → T"
        writeText library original
        expectFreshHover alsPath dir doc onF,
      testCase "an import moves into a dependency" $ withFixtures alsPath "HoverLibrary" "als-hover-depend" $ \dir -> do
        doc <- openDoc ("src" </> "Main.agda") "agda"
        expectHover doc onF "T → T"
        writeText (dir </> ".agda" </> "libraries") (Text.pack (dir </> "dep" </> "dep.agda-lib") <> "\n")
        liftIO $ removeFile (dir </> "src" </> "Dep.agda")
        writeText (dir </> "hover-library.agda-lib") "name: hover-library\ninclude: src\ndepend: dep\n"
        expectHover doc onF "T → T → T",
      testCase "the libraries file stops listing a dependency" $ withFixtures alsPath "HoverLibrary" "als-hover-libraries" $ \dir -> do
        let libraries = dir </> ".agda" </> "libraries"
        writeText libraries (Text.pack (dir </> "dep" </> "dep.agda-lib") <> "\n")
        liftIO $ removeFile (dir </> "src" </> "Dep.agda")
        writeText (dir </> "hover-library.agda-lib") "name: hover-library\ninclude: src\ndepend: dep\n"
        doc <- openDoc ("src" </> "Main.agda") "agda"
        expectHover doc onF "T → T → T"
        writeText libraries ""
        expectFreshHover alsPath dir doc onF,
      testCase "a new .agda-lib next to the hovered file" $ withFixtures alsPath "HoverLibrary" "als-hover-new-library" $ \dir -> do
        doc <- openDoc ("src" </> "Main.agda") "agda"
        expectHover doc onF "T → T"
        liftIO $ removeFile (dir </> "src" </> "Dep.agda")
        expectFreshHover alsPath dir doc onF
        writeText (dir </> "src" </> "near.agda-lib") "name: near\ninclude: . ../other\n"
        expectHover doc onF "T → T → T"
    ]
      -- the tool is a shell script, which Windows cannot run
      ++ [ testCase "remove a trusted executable, then add it back" $ withFixtures alsPath "HoverLibrary" "als-hover-executables" $ \dir -> do
             let tool = dir </> "tool"
                 executables = dir </> ".agda" </> "executables"
             writeText tool "#!/bin/sh\necho done\n"
             liftIO $ getPermissions tool >>= setPermissions tool . setOwnerExecutable True
             writeText executables (Text.pack tool <> "\n")
             doc <- openDoc ("src" </> "UsesTool.agda") "agda"
             expectHover doc onToolF "T → T"
             writeText executables ""
             expectFreshHover alsPath dir doc onToolF
             writeText executables (Text.pack tool <> "\n")
             expectHover doc onToolF "T → T"
           | os /= "mingw32"
         ]
  where
    -- UsesTool.agda starts with an OPTIONS pragma, so 'g = f' is one line lower
    onToolF = Position 6 4

-- | Like 'hoverAfterEdits', but for what an editor does in one session:
-- unsaved edits, switching files, loading, and hovering on different
-- kinds of token. The hover loads the file on disk and reads the token
-- from the editor's text.
hoverInSession :: FilePath -> TestTree
hoverInSession alsPath =
  testGroup
    "hover across unsaved edits and file switches"
    [ testCase "an unsaved edit, then an import edit" $ withFixtures alsPath "Hover" "als-hover-unsaved" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        main <- readText (dir </> "Main.agda")
        edit doc (main <> "\nh : T\nh = f t\n")
        expectHover doc onF "T → T"
        expectHover doc onUnsavedF "T → T"
        dep <- readText (dir </> "Dep.agda")
        writeText (dir </> "Dep.agda") (addArgumentToF dep)
        expectHover doc onUnsavedF "T → T → T",
      testCase "an unsaved edit, then discarded" $ withFixtures alsPath "Hover" "als-hover-discard" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        main <- readText (dir </> "Main.agda")
        edit doc (Text.replace "\n\ng : _" "\ng : _" main)
        expectHover doc (Position 4 4) "T → T"
        edit doc main
        expectHover doc onF "T → T",
      testCase "switch between files" $ withFixtures alsPath "Hover" "als-hover-switch" $ \dir -> do
        main <- openDoc "Main.agda" "agda"
        viaMid <- openDoc "ViaMid.agda" "agda"
        expectHover main onF "T → T"
        expectHover viaMid onF "T → T"
        expectHover main onF "T → T"
        dep <- readText (dir </> "Dep.agda")
        writeText (dir </> "Dep.agda") (addArgumentToF dep)
        expectHover viaMid onF "T → T → T"
        expectHover main onF "T → T → T",
      testCase "load between hovers" $ withFixtures alsPath "Hover" "als-hover-load" $ \dir -> do
        doc <- openDoc "Main.agda" "agda"
        expectHover doc onF "T → T"
        load doc
        expectHover doc onF "T → T"
        dep <- readText (dir </> "Dep.agda")
        writeText (dir </> "Dep.agda") (addArgumentToF dep)
        load doc
        expectHover doc onF "T → T → T",
      testCase "kinds of token" $ withFixtures alsPath "Hover" "als-hover-tokens" $ \dir -> do
        doc <- openDoc "Tokens.agda" "agda"
        -- a name from an import
        expectHover doc (Position 4 6) "Set"
        -- a name with a Unicode character
        expectHover doc (Position 8 1) "T → T"
        -- a qualified name
        expectHover doc (Position 14 12) "T → T"
        -- an operator, a local variable and a name from a where block,
        -- which the hover does not find in the scope of the module, and
        -- a keyword, which does not parse as an expression
        expectFreshHover alsPath dir doc (Position 11 8)
        expectFreshHover alsPath dir doc (Position 11 6)
        expectFreshHover alsPath dir doc (Position 11 11)
        expectFreshHover alsPath dir doc (Position 12 3)
    ]
  where
    -- 'f' in the line 'h = f t' that the unsaved edit adds to Main.agda
    onUnsavedF = Position 8 4

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

-- | With --no-hover-cache, which turns the cache off, the second hover
-- loads the files again and writes the .agdai files again
hoverWithoutCache :: FilePath -> IO ()
hoverWithoutCache alsPath = withFixtures (alsPath ++ " --no-hover-cache") "Hover" "als-hover-no-cache" $ \dir -> do
  doc <- openDoc "Main.agda" "agda"
  expectHover doc onF "T → T"
  written <- liftIO $ interfaceFiles dir
  liftIO $ map takeFileName written @?= ["Dep.agdai", "Main.agdai"]
  liftIO $ mapM_ removeFile written
  expectHover doc onF "T → T"
  writtenAgain <- liftIO $ interfaceFiles dir
  liftIO $ map takeFileName writtenAgain @?= ["Dep.agdai", "Main.agdai"]

--------------------------------------------------------------------------------
-- Helpers

-- | The .agdai files in a directory and its subdirectories, sorted
interfaceFiles :: FilePath -> IO [FilePath]
interfaceFiles dir = do
  entries <- sort <$> listDirectory dir
  concat <$> mapM inEntry entries
  where
    inEntry entry = do
      let path = dir </> entry
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then interfaceFiles path
        else return [path | takeExtension path == ".agdai"]

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
  edit doc text
  writeText (docPath doc) text

-- | Changes an open file in the editor only, without saving it
edit :: TextDocumentIdentifier -> Text -> Session ()
edit doc text = changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument text))]

-- | Loads an open file with 'Cmd_load', the way agda-mode does
load :: TextDocumentIdentifier -> Session ()
load doc = do
  let file = show (docPath doc)
  _ <- runIOTCM ("IOTCM " ++ file ++ " NonInteractive Direct( Cmd_load " ++ file ++ " [] )")
  return ()

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
