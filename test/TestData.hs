{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module TestData
  ( documentSymbolMessage,
    getModel,
    fileUri1,
    fileUri2,
    fileUri3,
    fakeUri,
    getServerEnv,
    AgdaFileDetails (..),
    agdaFileDetails,
    parseSourceFromPath,
    parseSourceFromPathAndContents,
  )
where

import Agda.Interaction.FindFile
  (
#if MIN_VERSION_Agda(2,8,0)
    SourceFile,
#else
    SourceFile (SourceFile),
#endif
  )
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Imports.Virtual (parseSourceFromContents)
import qualified Agda.Interaction.Options
import Agda.Syntax.Abstract.More ()
import Agda.Syntax.Common.Pretty (prettyShow)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.FileName (absolute)
import Agda.Utils.IORef (newIORef)
import Control.Concurrent (newChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Text (Text)
import Indexer (indexFile, usingSrcAsCurrent)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (Env (Env), catchTCError, runServerT)
import Options (defaultOptions, initConfig)
import Server.AgdaProjectResolver (findAgdaProject)
import qualified Server.CommandController as CommandController
import qualified Server.Filesystem as FS
import Server.Model (Model (Model))
import Server.Model.AgdaFile (AgdaFile, emptyAgdaFile)
import Server.Model.AgdaLib (AgdaLib (AgdaLib))
import qualified Server.Model.AgdaProject as AgdaProject
import Server.Model.Monad (MonadAgdaProject, runWithAgdaProjectT)
import qualified Server.ResponseController as ResponseController
import qualified Server.VfsIndex as VfsIndex
import System.FilePath (takeBaseName)
import Agda.Utils.Null (empty)

data AgdaFileDetails = AgdaFileDetails
  { fileName :: !String,
    agdaFile :: !AgdaFile,
    interface :: !TCM.Interface
  }

agdaFileDetails :: FilePath -> IO AgdaFileDetails
agdaFileDetails inPath = do
  let testName = takeBaseName inPath
      uri = LSP.filePathToUri inPath
  model <- TestData.getModel

  (file, interface) <- LSP.runLspT undefined $ do
    env <- TestData.getServerEnv model
    runServerT env $ do
      project <- findAgdaProject uri
      let withSrc f = runWithAgdaProjectT project $ do
            TCM.liftTCM $ TCM.setCommandLineOptions Agda.Interaction.Options.defaultOptions
            src <- parseSourceFromPath inPath

            f src

      let onErr = \err -> runWithAgdaProjectT project $ do
            t <- TCM.liftTCM $ prettyTCM err
            error $ prettyShow t

      interface <-
        ( withSrc $ \src -> usingSrcAsCurrent src $ do
            checkResult <- TCM.liftTCM $ Imp.typeCheckMain Imp.TypeCheck src
            return $ Imp.crInterface checkResult
        )
          `catchTCError` onErr

      file <- withSrc indexFile `catchTCError` onErr

      return (file, interface)

  return $ AgdaFileDetails testName file interface

sourceFileFromPath :: (TCM.MonadTCM m) => FilePath -> m SourceFile
sourceFileFromPath path = do
  absPath <- liftIO $ absolute path
#if MIN_VERSION_Agda(2,8,0)
  TCM.liftTCM $ TCM.srcFromPath absPath
#else
  return $ SourceFile absPath
#endif

parseSourceFromPath :: (TCM.MonadTCM m) => FilePath -> m Imp.Source
parseSourceFromPath path = do
  srcFile <- sourceFileFromPath path
  TCM.liftTCM $ Imp.parseSource srcFile

parseSourceFromPathAndContents ::
  (TCM.MonadTCM m, TCM.MonadTrace m, MonadAgdaProject m) =>
  FilePath ->
  Text ->
  m Imp.Source
parseSourceFromPathAndContents path contents = do
  srcFile <- sourceFileFromPath path
  let uri = LSP.toNormalizedUri $ LSP.filePathToUri path
  parseSourceFromContents uri srcFile contents

--------------------------------------------------------------------------------

documentSymbolMessage :: LSP.NormalizedUri -> LSP.TRequestMessage LSP.Method_TextDocumentDocumentSymbol
documentSymbolMessage uri =
  let params =
        LSP.DocumentSymbolParams
          Nothing
          Nothing
          (LSP.TextDocumentIdentifier $ LSP.fromNormalizedUri uri)
   in LSP.TRequestMessage
        "2.0"
        (LSP.IdInt 0)
        LSP.SMethod_TextDocumentDocumentSymbol
        params

--------------------------------------------------------------------------------

fileUri1 :: LSP.NormalizedUri
fileUri1 = LSP.toNormalizedUri $ LSP.Uri "file:///home/user2/project2/A/B/C.agda"

fileUri2 :: LSP.NormalizedUri
fileUri2 = LSP.toNormalizedUri $ LSP.Uri "file:///home/user/project2/X.agda"

fileUri3 :: LSP.NormalizedUri
fileUri3 = LSP.toNormalizedUri $ LSP.Uri "https://example.com/agda/Main.agda"

fakeUri :: LSP.NormalizedUri
fakeUri = LSP.toNormalizedUri $ LSP.Uri "file:///home/user2/project/Test.agda"

getModel :: IO Model
getModel = do
  let includes1 =
        FS.Uri . LSP.toNormalizedUri . LSP.Uri
          <$> [ "file:///home/user/project1/",
                "file:///home/user2/project2/",
                "https://example.com/agda/"
              ]
  let testLib1 = AgdaLib empty includes1 mempty empty (FS.LocalFilePath "")
  testProject1 <- AgdaProject.new (FS.Uri . LSP.toNormalizedUri . LSP.Uri $ "file:///home/user/project1/") (Just testLib1)

  let includes2 =
        FS.Uri . LSP.toNormalizedUri . LSP.Uri
          <$> ["file:///home/user/project2/"]
  let testLib2 = AgdaLib empty includes2 mempty empty (FS.LocalFilePath "")
  testProject2 <- AgdaProject.new (FS.Uri . LSP.toNormalizedUri . LSP.Uri $ "file:///home/user/project2/") (Just testLib2)

  let projects = [testProject1, testProject2]

  let testFile1 = emptyAgdaFile
  let testFile2 = emptyAgdaFile
  let testFile3 = emptyAgdaFile

  let files =
        Map.fromList
          [ (fileUri1, testFile1),
            (fileUri2, testFile2),
            (fileUri3, testFile3)
          ]

  return $ Model empty projects files

--------------------------------------------------------------------------------

getServerEnv :: (MonadIO m) => Model -> m Env
getServerEnv model =
  Env defaultOptions True initConfig True
    <$> liftIO newChan
    <*> liftIO CommandController.new
    <*> liftIO newChan
    <*> liftIO ResponseController.new
    <*> (pure $ FS.Layered [FS.Wrap FS.OsFilesystem])
    <*> liftIO (newIORef VfsIndex.empty)
    <*> liftIO (newIORef model)
