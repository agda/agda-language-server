{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.LSP (tests) where

import Agda
import qualified Agda.IR as IR
import Control.Exception (bracket)
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import GHC.TypeLits (symbolVal)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Switchboard (agdaCustomMethod)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, makeAbsolute, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (CreateProcess (cwd))
import Test.Tasty
import Test.Tasty.HUnit

tests :: FilePath -> TestTree
tests alsPath =
  testGroup
    "LSP"
    [ testCase "load" (demo alsPath),
      testCase "compile with the GHC and GHCNoMain backends" (testCompile alsPath),
      testCase "compile with the HTML backend after an invalid Agda option" (testCompileAfterInvalidOption alsPath)
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

-- | Compiles a fixture with both GHC backend settings and expects
-- 'DisplayInfoCompilationOk' for each. Before #99 was fixed, als never
-- registered Agda's builtin backends, so both failed with "No backend
-- called 'GHC'". '--ghc-dont-call-ghc' stops after generating the
-- Haskell code, so this doesn't need a working GHC, and '--compile-dir'
-- keeps the generated MAlonzo tree out of test/data.
testCompile :: FilePath -> IO ()
testCompile alsPath = withTempDir "als-test-compile" $ \compileDir -> do
  let server = alsPath ++ " +AGDA --ghc-dont-call-ghc --compile-dir=" ++ compileDir ++ " -AGDA"
  runSession server fullLatestClientCaps "test/data/" $ do
    ghc <- displayInfos <$> runIOTCM "IOTCM \"test/data/A.agda\" NonInteractive Direct( Cmd_compile GHC \"test/data/A.agda\" [] )"
    liftIO $ ghc @?= [JSON.toJSON (IR.ResponseDisplayInfo (IR.DisplayInfoCompilationOk [noMainWarning] []))]

    ghcNoMain <- displayInfos <$> runIOTCM "IOTCM \"test/data/A.agda\" NonInteractive Direct( Cmd_compile GHCNoMain \"test/data/A.agda\" [] )"
    liftIO $ ghcNoMain @?= [JSON.toJSON (IR.ResponseDisplayInfo (IR.DisplayInfoCompilationOk [] []))]
  where
    -- A.agda has no 'main', so the GHC backend warns about it
    noMainWarning :: String
#if MIN_VERSION_Agda(2,7,0)
    noMainWarning = "warning: -W[no]NoMain\nNo main function defined in A.\nUse option --no-main to suppress this warning."
#else
    noMainWarning = "warning: -W[no]GenericWarning\nNo main function defined in A. Use --no-main to suppress this\nwarning."
#endif

-- | Like 'testCompile', but with an Agda option that fails to parse. als
-- then falls back to the default options, and it must still register
-- Agda's builtin backends. The fallback also drops '--ghc-dont-call-ghc'
-- and '--compile-dir', so this compiles with the HTML backend instead,
-- which goes through the same backend lookup but needs no GHC. The
-- server runs in a temporary directory, so the generated html/ tree
-- stays out of the repo.
testCompileAfterInvalidOption :: FilePath -> IO ()
testCompileAfterInvalidOption alsPath = withTempDir "als-test-html" $ \dir -> do
  file <- makeAbsolute "test/data/A.agda"
  let server = alsPath ++ " +AGDA --no-such-option -AGDA"
      inDir process = process {cwd = Just dir}
  runSessionWithConfigCustomProcess inDir defaultConfig server fullLatestClientCaps "test/data/" $ do
    html <- displayInfos <$> runIOTCM ("IOTCM " ++ show file ++ " NonInteractive Direct( Cmd_compile HTML " ++ show file ++ " [] )")
    liftIO $ html @?= [JSON.toJSON (IR.ResponseDisplayInfo (IR.DisplayInfoCompilationOk [] []))]

-- | Keeps only the 'ResponseDisplayInfo' responses
displayInfos :: [JSON.Value] -> [JSON.Value]
displayInfos = filter isDisplayInfo
  where
    isDisplayInfo (JSON.Object o) = KeyMap.lookup "tag" o == Just "ResponseDisplayInfo"
    isDisplayInfo _ = False

-- | Runs an action with a fresh directory of the given name under the
-- system's temporary directory, and removes it afterwards
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name = bracket create removeDirectoryRecursive
  where
    create = do
      tmp <- getTemporaryDirectory
      let dir = tmp </> name
      createDirectoryIfMissing True dir
      return dir

-- | Sends an IOTCM command over the custom method, then answers every
-- request the server sends back on that method (the server waits for
-- each one to be answered before ending the command), until
-- 'ResponseEnd'. Returns the params of those requests, 'ResponseEnd'
-- excluded.
runIOTCM :: String -> Session [JSON.Value]
runIOTCM cmd = do
  _ <- sendRequest agdaCustomMethod (JSON.toJSON (CmdReq cmd))
  collect []
  where
    collect :: [JSON.Value] -> Session [JSON.Value]
    collect acc = do
      msg <- anyMessage
      case msg of
        FromServerMess (SMethod_CustomMethod p) (ReqMess (TRequestMessage _ reqId _ params))
          | symbolVal p == "agda" -> do
              sendResponse (TResponseMessage "2.0" (Just reqId) (Right JSON.Null))
              if params == JSON.toJSON IR.ResponseEnd
                then return (reverse acc)
                else collect (params : acc)
        _ -> collect acc

-- | Sends a custom method request to the server and expects a response of `CmdRes Nothing`
testCustomMethod :: String -> Session ()
testCustomMethod cmd = do
  TResponseMessage _ _ rsp <-
    request agdaCustomMethod $
      JSON.toJSON $
        CmdReq cmd
  liftIO $ rsp @?= Right (JSON.toJSON (CmdRes Nothing))
