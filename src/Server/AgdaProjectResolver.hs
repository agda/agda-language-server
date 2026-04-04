module Server.AgdaProjectResolver (findAgdaProject) where

import Agda.Utils.Maybe (caseMaybe, fromMaybe, fromMaybeM, ifJustM, listToMaybe)
import Monad (ServerM, askFilesystemProvider, askModel, modifyModel)
import qualified Server.AgdaLibResolver as AgdaLibResolver
import qualified Server.Filesystem as FS
import qualified Server.Model as Model
import Server.Model.AgdaProject (AgdaProject)
import qualified Server.Model.AgdaProject as AgdaProject

-- | Find cached 'AgdaProject', or else make one from @.agda-lib@ files on the
-- file system, or else provide a default
findAgdaProject :: (FS.IsFileId f) => f -> ServerM AgdaProject
findAgdaProject agdaFile' = do
  let agdaFile = FS.toFileId agdaFile'
  model <- askModel
  provider <- askFilesystemProvider
  root <- projectRoot provider agdaFile
  ifJustM (Model.getKnownAgdaProject agdaFile model) return $ do
    ProjectRoot rootFileId agdaLibFileId <- projectRoot provider agdaFile
    agdaLib <- caseMaybe agdaLibFileId (pure Nothing) AgdaLibResolver.byFileId
    project <- AgdaProject.new rootFileId agdaLib
    modifyModel $ Model.withAgdaProject project
    return project

data ProjectRoot = ProjectRoot
  { rootFileId :: !FS.FileId,
    agdaLibFileId :: !(Maybe FS.FileId)
  }

projectRoot :: (FS.MonadFilesystem m, FS.Provider p) => p -> FS.FileId -> m ProjectRoot
projectRoot provider child = do
  agdaLib <- ancestorAgdaLib provider child
  let base = fromMaybe child agdaLib
  rootDir <- fromMaybeM (pure base) $ FS.fileIdParent base
  return $ ProjectRoot rootDir agdaLib

ancestorAgdaLib :: (FS.MonadFilesystem m, FS.Provider p) => p -> FS.FileId -> m (Maybe FS.FileId)
ancestorAgdaLib provider childFileId = do
  parentFileId <- FS.fileIdParent childFileId
  caseMaybe parentFileId (return Nothing) $ \parentFileId -> do
    children <- FS.getChildren provider parentFileId
    let candidates = filter (\child -> FS.fileIdExtension child == ".agda-lib") children
    case listToMaybe candidates of
      Nothing -> ancestorAgdaLib provider parentFileId
      Just candidate -> return $ Just candidate
