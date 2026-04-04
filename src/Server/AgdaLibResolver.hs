module Server.AgdaLibResolver
  ( byFileId,
    byLibName,
    installedLibraries,
  )
where

import Agda.Interaction.Library (AgdaLibFile, LibName, findLib')
import Agda.Interaction.Library.Base (libName, libNameForCurrentDir)
import Agda.Interaction.Library.More (defaultLibraryFileIds)
import Agda.Interaction.Library.Parse.More (parseLibFile, runP)
import Agda.Setup (getAgdaAppDir)
import Agda.Utils.Either (maybeRight)
import Agda.Utils.Lens ((^.))
import Agda.Utils.List (nubOn)
import Agda.Utils.Maybe (caseMaybeM, listToMaybe)
import Agda.Utils.Monad (mapMaybeM)
import Agda.Utils.Singleton (singleton)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Monad (ServerM, askFilesystemProvider, askModel, modifyModel)
import qualified Server.Filesystem as FS
import qualified Server.Model as Model
import Server.Model.AgdaLib (AgdaLib, agdaLibFromFile, agdaLibName)
import Server.Model.InstalledLibsFile (InstalledLibsFile)
import qualified Server.Model.InstalledLibsFile as InstalledLibsFile

-- | Get an 'AgdaLib' given the 'FS.FileId' of its @.agda-lib@ file
byFileId :: FS.FileId -> ServerM (Maybe AgdaLib)
byFileId agdaLibFileId = do
  model <- askModel
  provider <- askFilesystemProvider
  caseMaybeM
    (fileIdToAgdaLibFile provider agdaLibFileId)
    (return Nothing)
    $ \agdaLibFile ->
      case Model.getKnownAgdaLib (agdaLibFile ^. libName) model of
        Just lib -> return $ Just lib
        Nothing -> do
          lib <- agdaLibFromFile agdaLibFile agdaLibFileId
          modifyModel $ Model.withAgdaLib lib
          return $ Just lib

-- | Get an 'AgdaLib' given its name and the list of installed libraries
byLibName :: LibName -> [AgdaLib] -> Maybe AgdaLib
byLibName libName = listToMaybe . findLib' (^. agdaLibName) libName

fileIdToAgdaLibFile :: (FS.Provider p) => p -> FS.FileId -> ServerM (Maybe AgdaLibFile)
fileIdToAgdaLibFile provider fileId = do
  agdaLibFile <- parseLibFile provider fileId
  case agdaLibFile of
    Nothing -> return Nothing
    Just agdaLibFile -> do
      let (result, _warnings) = runP agdaLibFile
      return $ maybeRight result

installedLibraries :: Maybe FS.FileId -> ServerM [AgdaLib]
installedLibraries overrideLibFile = do
  provider <- askFilesystemProvider
  libsFile <- librariesFile provider overrideLibFile
  let entries = maybe [] (^. InstalledLibsFile.entries) libsFile
  let entries' = nubOn (^. InstalledLibsFile.entryLibFileId) entries
  mapMaybeM
    (\entry -> byFileId (entry ^. InstalledLibsFile.entryLibFileId))
    entries'

librariesFile :: (FS.MonadFilesystem m, FS.Provider p) => p -> Maybe FS.FileId -> m (Maybe InstalledLibsFile)
librariesFile provider overrideLibFile = do
  agdaDir <- liftIO $ FS.LocalFilePath <$> getAgdaAppDir
  defaults <- defaultLibraryFileIds agdaDir
  let candidates = toList $ maybe defaults singleton overrideLibFile
  libsFiles <- mapMaybeM (InstalledLibsFile.fromFileId provider) candidates
  return $ listToMaybe libsFiles

-- | Determine the libraries we depend on when there is no .agda-lib
-- TODO: read from default file
defaultLibNames :: (FS.MonadFilesystem m, FS.Provider p) => p -> Bool -> m [LibName]
defaultLibNames provider useDefaultsFile =
  if useDefaultsFile
    then return [libNameForCurrentDir]
    else return []
