{-# LANGUAGE CPP #-}

module Indexer
  ( withAstFor,
    usingSrcAsCurrent,
    indexFile,
  )
where

#if MIN_VERSION_Agda(2,8,0)
#else
import Agda.Interaction.FindFile (srcFilePath)
#endif
import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.Imports.More as Imp
import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Translation.ConcreteToAbstract (ToAbstract (toAbstract), TopLevel (TopLevel), TopLevelInfo)
import qualified Agda.TypeChecking.Monad as TCM
import qualified Data.Map as Map
import Indexer.Indexer (indexAst)
import Indexer.Monad (execIndexerM)
import Indexer.Postprocess (postprocess)
import qualified Server.Log as Log
import Server.Model.AgdaFile (AgdaFile)
import Server.Model.Monad (WithAgdaProjectM)
import Indexer.Prepare (setCommandLineOptionsByLib)
import Agda.Syntax.Common.Pretty (pretty)
import Agda.Interaction.Options (defaultOptions)

usingSrcAsCurrent :: Imp.Source -> WithAgdaProjectM a -> WithAgdaProjectM a
usingSrcAsCurrent src x = do
  TCM.liftTCM TCM.resetState

  setCommandLineOptionsByLib defaultOptions

  TCM.liftTCM $ Imp.setOptionsFromSourcePragmas True src

  TCM.liftTCM Imp.importPrimitiveModules

  TCM.setCurrentRange (C.modPragmas . Imp.srcModule $ src) $ do
    persistentOptions <- TCM.stPersistentOptions . TCM.stPersistentState <$> TCM.getTC
    setCommandLineOptionsByLib persistentOptions

  TCM.liftTCM $ Imp.setOptionsFromSourcePragmas True src

#if MIN_VERSION_Agda(2,8,0)
  TCM.modifyTCLens TCM.stModuleToSourceId $ Map.insert (Imp.srcModuleName src) (Imp.srcOrigin src)
  TCM.localTC (\e -> e {TCM.envCurrentPath = Just (TCM.srcFileId $ Imp.srcOrigin src)}) x
#else
  TCM.modifyTCLens TCM.stModuleToSource $ Map.insert (Imp.srcModuleName src) (srcFilePath $ Imp.srcOrigin src)
  TCM.localTC (\e -> e {TCM.envCurrentPath = Just (srcFilePath $ Imp.srcOrigin src)}) x
#endif

withAstFor :: Imp.Source -> (TopLevelInfo -> WithAgdaProjectM a) -> WithAgdaProjectM a
withAstFor src f = usingSrcAsCurrent src $ do
#if MIN_VERSION_Agda(2,8,0)
  let srcFile = Imp.srcOrigin src
#else
  let srcFile = srcFilePath $ Imp.srcOrigin src
#endif
  let topLevel =
        TopLevel
          srcFile
          (Imp.srcModuleName src)
          (C.modDecls $ Imp.srcModule src)
  ast <- TCM.liftTCM $ toAbstract topLevel
  f ast

indexFile :: Imp.Source -> WithAgdaProjectM AgdaFile
indexFile src = do
  Log.infoP $ "Indexing " <> pretty src
  withAstFor src $ \ast -> execIndexerM $ do
    indexAst ast
    postprocess
