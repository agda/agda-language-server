{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Agda.Interaction.Imports.Virtual
  ( VSourceFile (..),
    vSrcFromUri,
    parseSourceFromContents,
    parseVSource,
  )
where

#if MIN_VERSION_Agda(2,8,0)
import Agda.TypeChecking.Monad (SourceFile)
#else
import Agda.Interaction.FindFile (SourceFile (SourceFile))
#endif
import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.Imports.More as Imp
import Agda.Syntax.Parser (moduleParser, parseFile)
import Agda.Syntax.Position (mkRangeFile)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.Maybe (maybeToList)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Strict as Strict
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidAbsolutePath)
import qualified Language.LSP.VFS as VFS
import Server.Model.AgdaLib (agdaLibToFile)
import Server.Model.Monad (MonadAgdaProject, askAgdaLib)

data VSourceFile = VSourceFile
  { vSrcFileSrcFile :: SourceFile,
    vSrcUri :: LSP.NormalizedUri,
    vSrcVFile :: VFS.VirtualFile
  }

#if MIN_VERSION_Agda(2,8,0)
vSrcFromUri ::
  (TCM.MonadFileId m, MonadIO m) =>
  LSP.NormalizedUri ->
  VFS.VirtualFile ->
  m VSourceFile
vSrcFromUri normUri file = do
  absPath <- uriToPossiblyInvalidAbsolutePath normUri
  src <- TCM.srcFromPath absPath
  return $ VSourceFile src normUri file
#else
vSrcFromUri ::
  (MonadIO m) =>
  LSP.NormalizedUri ->
  VFS.VirtualFile ->
  m VSourceFile
vSrcFromUri normUri file = do
  absPath <- uriToPossiblyInvalidAbsolutePath normUri
  let src = SourceFile absPath
  return $ VSourceFile src normUri file
#endif

parseSourceFromContents ::
  (TCM.MonadTrace m, MonadAgdaProject m) =>
  LSP.NormalizedUri ->
  SourceFile ->
  Text.Text ->
  m Imp.Source
parseSourceFromContents srcUri srcFile contentsStrict = do
  f <- TCM.liftTCM $ Imp.srcFilePath srcFile

  let rf0 = mkRangeFile f Nothing
  TCM.setCurrentRange (Imp.beginningOfFile rf0) $ do
    let contents = Strict.toLazy contentsStrict
    let contentsString = Text.unpack contentsStrict

    parsedModName0 <-
      TCM.liftTCM $
        Imp.moduleName f . fst . fst =<< do
          Imp.runPMDropWarnings $ parseFile moduleParser rf0 contentsString

    let rf = mkRangeFile f $ Just parsedModName0
    ((parsedMod, attrs), fileType) <- TCM.liftTCM $ Imp.runPM $ parseFile moduleParser rf contentsString
    parsedModName <- TCM.liftTCM $ Imp.moduleName f parsedMod

    agdaLib <- askAgdaLib
    let libs = maybeToList $ agdaLibToFile srcUri <$> agdaLib

    return
      Imp.Source
        { Imp.srcText = contents,
          Imp.srcFileType = fileType,
          Imp.srcOrigin = srcFile,
          Imp.srcModule = parsedMod,
          Imp.srcModuleName = parsedModName,
          Imp.srcProjectLibs = libs,
          Imp.srcAttributes = attrs
        }

-- | Based on @parseSource@
parseVSource :: (TCM.MonadTCM m, TCM.MonadTrace m, MonadAgdaProject m) => VSourceFile -> m Imp.Source
parseVSource (VSourceFile srcFile uri vFile) =
  parseSourceFromContents uri srcFile (VFS.virtualFileText vFile)
