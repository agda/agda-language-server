{-# LANGUAGE CPP #-}

module Server.Model.AgdaProject
  ( AgdaProject,
    new,
    projectRoot,
    agdaLib,
    tcStateRef,
    tcEnv,
  )
where

import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.IORef (IORef, newIORef)
import Agda.Utils.Lens (Lens', (<&>), (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Server.Model.AgdaLib (AgdaLib)
import Agda.Syntax.Common.Pretty (Pretty, pretty, text, (<+>))
import qualified Server.Filesystem as FS

data Origin = FromLib !FS.FileId | Defaulted
  deriving (Show, Eq)

data AgdaProject = AgdaProject
  {
    _projectRoot :: !(FS.FileId),
    _agdaLib :: !(Maybe AgdaLib),
    _tcStateRef :: !(IORef TCM.TCState),
    _tcEnv :: !TCM.TCEnv
  }

instance Pretty AgdaProject where
  pretty agdaProject =
    text "AgdaProject"
      <+> pretty (agdaProject ^. agdaLib)

new :: (MonadIO m) => FS.FileId -> Maybe AgdaLib -> m AgdaProject
new projectRoot agdaLib = do
#if MIN_VERSION_Agda(2,8,0)
  tcState <- liftIO TCM.initStateIO
#else
  let tcState = TCM.initState
#endif
  let persistentState = TCM.stPersistentState tcState
  -- Prevent Agda from writing to standard output by default. LSP likes to use
  -- it, so Agda shouldn't try to use it too.
  let tcState' = tcState { TCM.stPersistentState = persistentState { TCM.stInteractionOutputCallback = \_ -> return () } }
  tcStateRef <- liftIO $ newIORef tcState'
  let tcEnv = TCM.initEnv
  return $ AgdaProject projectRoot agdaLib tcStateRef tcEnv

projectRoot :: Lens' AgdaProject FS.FileId
projectRoot f a = f (_projectRoot a) <&> \x -> a {_projectRoot = x}

agdaLib :: Lens' AgdaProject (Maybe AgdaLib)
agdaLib f a = f (_agdaLib a) <&> \x -> a {_agdaLib = x}

tcStateRef :: Lens' AgdaProject (IORef TCM.TCState)
tcStateRef f a = f (_tcStateRef a) <&> \x -> a {_tcStateRef = x}

tcEnv :: Lens' AgdaProject TCM.TCEnv
tcEnv f a = f (_tcEnv a) <&> \x -> a {_tcEnv = x}
