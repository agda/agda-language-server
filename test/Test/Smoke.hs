module Test.Smoke (tests) where

import Agda
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Language.LSP.Protocol.Message (SMethod (..), TResponseMessage (..))
import Language.LSP.Protocol.Types (HoverParams (..), Position (..))
import Language.LSP.Test (fullLatestClientCaps, openDoc, request, runSession)
import Switchboard (agdaCustomMethod)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

-- | Smoke tests against a given 'als' executable. Point '--als-path' at
-- a packaged release artifact (not the in-place '.stack-work' build) to
-- catch packaging bugs -- like #6, where the Linux release ships no
-- bundled ICU libs -- that the rest of the suite can't see, since it
-- runs against a binary on the very machine that built it, where the
-- system ICU version trivially matches by construction.
tests :: FilePath -> TestTree
tests alsPath =
  testGroup
    "Smoke"
    [ testCase "als --version" (testVersion alsPath),
      testCase "open a non-ASCII .agda file over LSP" (testUnicodeFile alsPath)
    ]

testVersion :: FilePath -> IO ()
testVersion alsPath = do
  (code, out, err) <- readProcessWithExitCode alsPath ["--version"] ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure n ->
      assertFailure $
        "'" ++ alsPath ++ " --version' exited with code " ++ show n
          ++ "\nstdout: " ++ out
          ++ "\nstderr: " ++ err

-- | Opens a fixture with non-ASCII identifiers, hovers over a real
-- symbol (mirrors Test.LSP's "load" test; a position with no symbol
-- under it, e.g. (0,0), never gets a response at all), then asks the
-- server to load it via the custom agda-mode protocol. If the process
-- can't even start (e.g. the dynamic-linking failure in #6), 'runSession'
-- itself throws before either request below is reached.
testUnicodeFile :: FilePath -> IO ()
testUnicodeFile alsPath =
  runSession alsPath fullLatestClientCaps "test/data/" $ do
    doc <- openDoc "Unicode.agda" "agda"
    -- hover over "double" on its type-signature line
    _ <- request SMethod_TextDocumentHover (HoverParams doc (Position 6 2) Nothing)
    TResponseMessage _ _ rsp <-
      request agdaCustomMethod $
        JSON.toJSON $
          -- The outer IOTCM path intentionally doesn't match the real
          -- file, mirroring Test.LSP's "load" test (which uses
          -- "A.agdaa" for the same field).
          CmdReq "IOTCM \"test/data/Unicode.agdaa\" NonInteractive Direct( Cmd_load \"test/data/Unicode.agda\" [] )"
    liftIO $ rsp @?= Right (JSON.toJSON (CmdRes Nothing))
