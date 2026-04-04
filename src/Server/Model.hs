module Server.Model
  ( Model (Model),
    mkEmpty,
    getKnownAgdaLib,
    getKnownAgdaProject,
    withAgdaLib,
    withAgdaProject,
    getAgdaFile,
    setAgdaFile,
    deleteAgdaFile,
  )
where

import Agda.Interaction.Library (LibName)
import Agda.Utils.Lens (Lens', over, (^.))
import Agda.Utils.Maybe (listToMaybe)
import Agda.Utils.Monad (filterM)
import Agda.Utils.Null (empty)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Types as LSP
import qualified Server.Filesystem as FS
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.AgdaLib (AgdaLib, agdaLibName)
import Server.Model.AgdaProject (AgdaProject)
import qualified Server.Model.AgdaProject as AgdaProject

data Model = Model
  { _modelAgdaLibs :: !(Map LibName AgdaLib),
    _modelAgdaProjects :: ![AgdaProject],
    _modelAgdaFiles :: !(Map LSP.NormalizedUri AgdaFile)
  }

mkEmpty :: Model
mkEmpty = Model empty empty empty

agdaLibs :: Lens' Model (Map LibName AgdaLib)
agdaLibs f a = f (_modelAgdaLibs a) <&> \x -> a {_modelAgdaLibs = x}

agdaProjects :: Lens' Model [AgdaProject]
agdaProjects f a = f (_modelAgdaProjects a) <&> \x -> a {_modelAgdaProjects = x}

agdaFiles :: Lens' Model (Map LSP.NormalizedUri AgdaFile)
agdaFiles f a = f (_modelAgdaFiles a) <&> \x -> a {_modelAgdaFiles = x}

getKnownAgdaLib :: LibName -> Model -> Maybe AgdaLib
getKnownAgdaLib libName model = Map.lookup libName $ model ^. agdaLibs

getKnownAgdaProject :: (MonadIO m) => FS.FileId -> Model -> m (Maybe AgdaProject)
getKnownAgdaProject agdaFileId model = do
  let projects = model ^. agdaProjects
  let projectMatches = \project -> FS.referToSameFile agdaFileId (project ^. AgdaProject.projectRoot)
  listToMaybe <$> filterM projectMatches projects

-- | Add an 'AgdaLib' to the model
withAgdaLib :: AgdaLib -> Model -> Model
withAgdaLib lib =
  let libName = lib ^. agdaLibName
   in over agdaLibs (Map.insert libName lib)

-- | Add an 'AgdaProject' to the model
withAgdaProject :: AgdaProject -> Model -> Model
withAgdaProject project = over agdaProjects (project :)

getAgdaFile :: LSP.NormalizedUri -> Model -> Maybe AgdaFile
getAgdaFile uri = Map.lookup uri . _modelAgdaFiles

setAgdaFile :: LSP.NormalizedUri -> AgdaFile -> Model -> Model
setAgdaFile uri agdaFile = over agdaFiles $ Map.insert uri agdaFile

deleteAgdaFile :: LSP.NormalizedUri -> Model -> Model
deleteAgdaFile uri = over agdaFiles $ Map.delete uri
