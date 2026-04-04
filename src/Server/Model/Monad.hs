{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Server.Model.Monad
  ( MonadAgdaProject (..),
    askAgdaLib,
    MonadAgdaFile (..),
    useAgdaFile,
    WithAgdaProjectT,
    runWithAgdaProjectT,
    WithAgdaProjectM,
    WithAgdaFileT,
    runWithAgdaFileT,
    WithAgdaFileM,
  )
where

import Agda.Interaction.Options (CommandLineOptions (optPragmaOptions), PragmaOptions)
import Agda.Syntax.Position (getRange)
import Agda.TypeChecking.Monad (HasOptions (..), MonadTCEnv (..), MonadTCM (..), MonadTCState (..), MonadTrace, PersistentTCState (stPersistentOptions), ReadTCState (..), TCEnv (..), TCM, TCMT (..), TCState (stPersistentState), modifyTCLens, setTCLens, stPragmaOptions, useTC)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.Utils.IORef (modifyIORef', readIORef, writeIORef)
import Agda.Utils.Lens (Lens', locally, over, view, (<&>), (^.))
import Agda.Utils.Monad (MonadTrans (lift), and2M, bracket_, ifNotM)
import Agda.Utils.Null (null)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask)
import Monad (MonadMockLsp, ServerM)
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.AgdaLib (AgdaLib)
import Server.Model.AgdaProject (AgdaProject)
import qualified Server.Model.AgdaProject as AgdaProject
import Prelude hiding (null)
#if MIN_VERSION_Agda(2,8,0)
import Agda.Utils.FileId (File, getIdFile)
import Language.LSP.Server (MonadLsp, getLspEnv)
import Options (Config)
import Control.Monad.IO.Unlift (MonadUnliftIO)
#endif

--------------------------------------------------------------------------------

class (MonadTCM m, ReadTCState m) => MonadAgdaProject m where
  askAgdaProject :: m AgdaProject
  localAgdaProject :: (AgdaProject -> AgdaProject) -> m a -> m a

useAgdaProject :: (MonadAgdaProject m) => Lens' AgdaProject a -> m a
useAgdaProject lens = do
  agdaProject <- askAgdaProject
  return $ agdaProject ^. lens

askAgdaLib :: (MonadAgdaProject m) => m (Maybe AgdaLib)
askAgdaLib = useAgdaProject AgdaProject.agdaLib

class (MonadAgdaProject m) => MonadAgdaFile m where
  askAgdaFile :: m AgdaFile
  localAgdaFile :: (AgdaFile -> AgdaFile) -> m a -> m a

useAgdaFile :: (MonadAgdaFile m) => Lens' AgdaFile a -> m a
useAgdaFile lens = do
  agdaFile <- askAgdaFile
  return $ agdaFile ^. lens

--------------------------------------------------------------------------------

defaultAskTC :: (MonadAgdaProject m) => m TCEnv
defaultAskTC = useAgdaProject AgdaProject.tcEnv

defaultLocalTC :: (MonadAgdaProject m) => (TCEnv -> TCEnv) -> m a -> m a
defaultLocalTC f = localAgdaProject (over AgdaProject.tcEnv f)

defaultGetTC :: (MonadAgdaProject m) => m TCState
defaultGetTC = do
  tcStateRef <- useAgdaProject AgdaProject.tcStateRef
  liftIO $ readIORef tcStateRef

defaultPutTC :: (MonadAgdaProject m) => TCState -> m ()
defaultPutTC tcState = do
  tcStateRef <- useAgdaProject AgdaProject.tcStateRef
  liftIO $ writeIORef tcStateRef tcState

defaultModifyTC :: (MonadAgdaProject m) => (TCState -> TCState) -> m ()
defaultModifyTC f = do
  tcStateRef <- useAgdaProject AgdaProject.tcStateRef
  liftIO $ modifyIORef' tcStateRef f

-- Taken from TCMT implementation
defaultLocallyTCState :: (MonadAgdaProject m) => Lens' TCState a -> (a -> a) -> m b -> m b
defaultLocallyTCState lens f = bracket_ (useTC lens <* modifyTCLens lens f) (setTCLens lens)

-- Taken from TCMT implementation
defaultPragmaOptionsImpl :: (MonadAgdaProject m) => m PragmaOptions
defaultPragmaOptionsImpl = useTC stPragmaOptions

-- Taken from TCMT implementation
defaultCommandLineOptionsImpl :: (MonadAgdaProject m) => m CommandLineOptions
defaultCommandLineOptionsImpl = do
  p <- useTC stPragmaOptions
  cl <- stPersistentOptions . stPersistentState <$> getTC
  return $ cl {optPragmaOptions = p}

defaultLiftTCM :: (MonadAgdaProject m) => TCM a -> m a
defaultLiftTCM (TCM f) = do
  tcStateRef <- useAgdaProject AgdaProject.tcStateRef
  tcEnv <- useAgdaProject AgdaProject.tcEnv
  liftIO $ f tcStateRef tcEnv

-- Taken from TCM implementation
defaultTraceClosureCall :: (MonadAgdaProject m, MonadTrace m) => TCM.Closure TCM.Call -> m a -> m a
defaultTraceClosureCall cl m = do
  -- Compute update to 'Range' and 'Call' components of 'TCEnv'.
  let withCall =
        localTC $
          foldr (.) id $
            concat $
              [ [\e -> e {envCall = Just cl} | TCM.interestingCall call],
                [ \e -> e {envHighlightingRange = callRange}
                | callHasRange && highlightCall
                    || isNoHighlighting
                ],
                [\e -> e {envRange = callRange} | callHasRange]
              ]

  -- For interactive highlighting, also wrap computation @m@ in 'highlightAsTypeChecked':
  ifNotM
    (pure highlightCall `and2M` do (TCM.Interactive ==) . envHighlightingLevel <$> askTC)
    {-then-} (withCall m)
    {-else-} $ do
      oldRange <- envHighlightingRange <$> askTC
      TCM.highlightAsTypeChecked oldRange callRange $
        withCall m
  where
    call = TCM.clValue cl
    callRange = getRange call
    callHasRange = not $ null callRange

    -- Should the given call trigger interactive highlighting?
    highlightCall = case call of
      TCM.CheckClause {} -> True
      TCM.CheckLHS {} -> True
      TCM.CheckPattern {} -> True
      TCM.CheckPatternLinearityType {} -> False
      TCM.CheckPatternLinearityValue {} -> False
      TCM.CheckLetBinding {} -> True
      TCM.InferExpr {} -> True
      TCM.CheckExprCall {} -> True
      TCM.CheckDotPattern {} -> True
      TCM.IsTypeCall {} -> True
      TCM.IsType_ {} -> True
      TCM.InferVar {} -> True
      TCM.InferDef {} -> True
      TCM.CheckArguments {} -> True
      TCM.CheckMetaSolution {} -> False
      TCM.CheckTargetType {} -> False
      TCM.CheckDataDef {} -> True
      TCM.CheckRecDef {} -> True
      TCM.CheckConstructor {} -> True
      TCM.CheckConArgFitsIn {} -> False
      TCM.CheckFunDefCall _ _ _ h -> h
      TCM.CheckPragma {} -> True
      TCM.CheckPrimitive {} -> True
      TCM.CheckIsEmpty {} -> True
      TCM.CheckConfluence {} -> False
      TCM.CheckIApplyConfluence {} -> False
      TCM.CheckModuleParameters {} -> False
      TCM.CheckWithFunctionType {} -> True
      TCM.CheckSectionApplication {} -> True
      TCM.CheckNamedWhere {} -> False
      TCM.ScopeCheckExpr {} -> False
      TCM.ScopeCheckDeclaration {} -> False
      TCM.ScopeCheckLHS {} -> False
      TCM.NoHighlighting {} -> True
      TCM.CheckProjection {} -> False
      TCM.SetRange {} -> False
      TCM.ModuleContents {} -> False

    isNoHighlighting = case call of
      TCM.NoHighlighting {} -> True
      _ -> False

#if MIN_VERSION_Agda(2,8,0)
-- Taken from TCMT implementation
defaultFileFromId :: (MonadAgdaProject m) => TCM.FileId -> m File
defaultFileFromId fi = useTC TCM.stFileDict <&> (`getIdFile` fi)

-- Taken from TCMT implementation
defaultIdFromFile :: (MonadAgdaProject m) => File -> m TCM.FileId
defaultIdFromFile = TCM.stateTCLens TCM.stFileDict . TCM.registerFileIdWithBuiltin
#endif

--------------------------------------------------------------------------------

newtype WithAgdaProjectT m a = WithAgdaProjectT {unWithAgdaProjectT :: ReaderT AgdaProject m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadUnliftIO)

runWithAgdaProjectT :: AgdaProject -> WithAgdaProjectT m a -> m a
runWithAgdaProjectT agdaProject = flip runReaderT agdaProject . unWithAgdaProjectT

type WithAgdaProjectM = WithAgdaProjectT ServerM

instance (MonadIO m) => MonadAgdaProject (WithAgdaProjectT m) where
  askAgdaProject = WithAgdaProjectT ask
  localAgdaProject f = WithAgdaProjectT . local f . unWithAgdaProjectT

instance (MonadMockLsp m) => MonadMockLsp (WithAgdaProjectT m)

instance (MonadIO m) => MonadTCEnv (WithAgdaProjectT m) where
  askTC = defaultAskTC
  localTC = defaultLocalTC

instance (MonadIO m) => MonadTCState (WithAgdaProjectT m) where
  getTC = defaultGetTC
  putTC = defaultPutTC
  modifyTC = defaultModifyTC

instance (MonadIO m) => ReadTCState (WithAgdaProjectT m) where
  getTCState = defaultGetTC
  locallyTCState = defaultLocallyTCState

instance (MonadIO m) => HasOptions (WithAgdaProjectT m) where
  pragmaOptions = defaultPragmaOptionsImpl
  commandLineOptions = defaultCommandLineOptionsImpl

-- TODO: how should this really be implemented?
instance (MonadIO m) => MonadTrace (WithAgdaProjectT m) where
  traceClosureCall = defaultTraceClosureCall
  printHighlightingInfo _ _ = return ()

instance (MonadIO m) => MonadTCM (WithAgdaProjectT m) where
  liftTCM = defaultLiftTCM

#if MIN_VERSION_Agda(2,8,0)
instance (MonadIO m) => TCM.MonadFileId (WithAgdaProjectT m) where
  fileFromId = defaultFileFromId
  idFromFile = defaultIdFromFile
#endif

instance (MonadLsp Config m) => MonadLsp Config (WithAgdaProjectT m) where
  getLspEnv = lift getLspEnv

--------------------------------------------------------------------------------

data WithAgdaFileEnv = WithAgdaFileEnv
  { _withAgdaFileEnvAgdaProject :: !AgdaProject,
    _withAgdaFileEnvAgdaFile :: !AgdaFile
  }

withAgdaFileEnvAgdaProject :: Lens' WithAgdaFileEnv AgdaProject
withAgdaFileEnvAgdaProject f a = f (_withAgdaFileEnvAgdaProject a) <&> \x -> a {_withAgdaFileEnvAgdaProject = x}

withAgdaFileEnvAgdaFile :: Lens' WithAgdaFileEnv AgdaFile
withAgdaFileEnvAgdaFile f a = f (_withAgdaFileEnvAgdaFile a) <&> \x -> a {_withAgdaFileEnvAgdaFile = x}

newtype WithAgdaFileT m a = WithAgdaFileT
  {unWithAgdaFileT :: ReaderT WithAgdaFileEnv m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runWithAgdaFileT :: AgdaProject -> AgdaFile -> WithAgdaFileT m a -> m a
runWithAgdaFileT agdaProject agdaFile =
  let env = WithAgdaFileEnv agdaProject agdaFile
   in flip runReaderT env . unWithAgdaFileT

type WithAgdaFileM = WithAgdaFileT ServerM

instance (MonadIO m) => MonadAgdaProject (WithAgdaFileT m) where
  askAgdaProject = WithAgdaFileT $ view withAgdaFileEnvAgdaProject
  localAgdaProject f = WithAgdaFileT . locally withAgdaFileEnvAgdaProject f . unWithAgdaFileT

instance (MonadIO m) => MonadAgdaFile (WithAgdaFileT m) where
  askAgdaFile = WithAgdaFileT $ view withAgdaFileEnvAgdaFile
  localAgdaFile f = WithAgdaFileT . locally withAgdaFileEnvAgdaFile f . unWithAgdaFileT

instance (MonadMockLsp m) => MonadMockLsp (WithAgdaFileT m)

instance (MonadIO m) => MonadTCEnv (WithAgdaFileT m) where
  askTC = defaultAskTC
  localTC = defaultLocalTC

instance (MonadIO m) => MonadTCState (WithAgdaFileT m) where
  getTC = defaultGetTC
  putTC = defaultPutTC
  modifyTC = defaultModifyTC

instance (MonadIO m) => ReadTCState (WithAgdaFileT m) where
  getTCState = defaultGetTC
  locallyTCState = defaultLocallyTCState

instance (MonadIO m) => HasOptions (WithAgdaFileT m) where
  pragmaOptions = defaultPragmaOptionsImpl
  commandLineOptions = defaultCommandLineOptionsImpl

instance (MonadIO m) => MonadTCM (WithAgdaFileT m) where
  liftTCM = defaultLiftTCM
