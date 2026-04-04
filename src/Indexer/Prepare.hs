module Indexer.Prepare (setCommandLineOptionsByLib) where

import Agda.Interaction.Library (LibName)
import Agda.Interaction.Library.Base (libNameForCurrentDir)
import Agda.Interaction.Options (CommandLineOptions (..))
import qualified Agda.Interaction.Options.Lenses as Lens
import Agda.Syntax.Common.Pretty (pretty)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.TypeChecking.Monad.Benchmark (updateBenchmarkingStatus)
import Agda.Utils.FileName (AbsolutePath, absolute)
import Agda.Utils.Lens ((^.))
import qualified Agda.Utils.List1 as List1
import Agda.Utils.Maybe (ifJustM, maybeToList)
import Agda.Utils.Monad (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Server.AgdaLibResolver as AgdaLibResolver
import qualified Server.Filesystem as FS
import qualified Server.Log as Log
import Server.Model.AgdaLib (AgdaLib, agdaLibDependencies, agdaLibIncludes, agdaLibName)
import Server.Model.Monad (MonadAgdaProject, WithAgdaProjectM, askAgdaLib)
import System.Directory (getCurrentDirectory)

setCommandLineOptionsByLib :: CommandLineOptions -> WithAgdaProjectM ()
setCommandLineOptionsByLib opts = do
  root <- liftIO (absolute =<< getCurrentDirectory)
  setCommandLineOptionsByLib' root opts

setCommandLineOptionsByLib' :: AbsolutePath -> CommandLineOptions -> WithAgdaProjectM ()
setCommandLineOptionsByLib' root opts = do
  incs <- case optAbsoluteIncludePaths opts of
    [] -> do
      opts' <- updateOptionsWithDependencyLibs opts
      let incs = optIncludePaths opts'
      TCM.liftTCM $ TCM.setIncludeDirs incs root
      List1.toList <$> TCM.getIncludeDirs
    incs -> return incs
  TCM.modifyTC $ Lens.setCommandLineOptions opts {optAbsoluteIncludePaths = incs}
  TCM.liftTCM $ TCM.setPragmaOptions (optPragmaOptions opts)
  TCM.liftTCM updateBenchmarkingStatus

-- | Determine the libraries we directly depend on
directDependencyLibNames ::
  (MonadAgdaProject m) =>
  -- | Persistent command line options
  CommandLineOptions ->
  m [LibName]
directDependencyLibNames o
  | not (null $ optLibraries o) = return $ optLibraries o
  | not (optUseLibs o) = return []
  | otherwise =
      ifJustM
        askAgdaLib
        (\agdaLib -> return $ agdaLib ^. agdaLibDependencies)
        (defaultLibNames o)

-- | Determine the libraries we depend on when there is no .agda-lib
-- TODO: read from default file
defaultLibNames :: (MonadAgdaProject m) => CommandLineOptions -> m [LibName]
defaultLibNames o =
  if optDefaultLibs o
    then return [libNameForCurrentDir]
    else return []

dependencyLibs :: CommandLineOptions -> WithAgdaProjectM [AgdaLib]
dependencyLibs o = do
  directDependencies <- directDependencyLibNames o
  Log.traceP $ "Direct dependencies: " <> pretty directDependencies
  installed <- lift $ AgdaLibResolver.installedLibraries (FS.LocalFilePath <$> optOverrideLibrariesFile o)
  Log.traceP $ "Installed libraries: " <> pretty installed
  let libs = resolveDeps installed directDependencies [] []
  Log.traceP $ "Resolved dependencies: " <> pretty libs
  case libs of
    Nothing -> return [] -- TODO: very wrong, do real error handling
    Just libs -> do
      projectLib <- askAgdaLib
      return $ maybeToList projectLib <> libs
  where
    resolveDeps :: [AgdaLib] -> [LibName] -> [LibName] -> [AgdaLib] -> Maybe [AgdaLib]
    resolveDeps _installed [] _doneNames doneLibs = Just doneLibs
    resolveDeps installed (next : todo) doneNames doneLibs
      | next `elem` doneNames = resolveDeps installed todo doneNames doneLibs
      | otherwise = do
          lib <- AgdaLibResolver.byLibName next installed
          let newDeps = lib ^. agdaLibDependencies
          resolveDeps installed (newDeps <> todo) (next : doneNames) (lib : doneLibs)

updateOptionsWithDependencyLibs :: CommandLineOptions -> WithAgdaProjectM CommandLineOptions
updateOptionsWithDependencyLibs o = do
  libs <- dependencyLibs o
  let includes = concatMap (^. agdaLibIncludes) libs
  let libNames = fmap (^. agdaLibName) libs
  let includePaths = FS.fileIdToPossiblyInvalidFilePath <$> includes
  return $
    o
      { optIncludePaths = includePaths <> optIncludePaths o,
        optLibraries = libNames
      }
