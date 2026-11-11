{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler where

import           Agda                           ( getCommandLineOptions
                                                , runAgda
                                                )
import qualified Agda.IR                       as IR

import           Agda.Interaction.Base          ( CommandQueue(..)
#if MIN_VERSION_Agda(2,7,0)
#else
                                                , CommandM
#endif
                                                , CommandState(optionsOnReload)
                                                , Rewrite(AsIs)
                                                , initCommandState
                                                )
import           Agda.Interaction.BasicOps      ( atTopLevel
                                                , typeInCurrent
                                                )
import           Agda.Interaction.Highlighting.Precise
                                                ( HighlightingInfo )
import qualified Agda.Interaction.Imports      as Imp
import           Agda.Interaction.InteractionTop
                                                ( cmd_load'
#if MIN_VERSION_Agda(2,8,0)
#else
                                                , localStateCommandM
#endif

#if MIN_VERSION_Agda(2,8,0)
#elif MIN_VERSION_Agda(2,7,0)
                                                , CommandM
#else
#endif
                                                )
import           Agda.Interaction.Options       ( CommandLineOptions
                                                  ( optAbsoluteIncludePaths
                                                  , optTrustedExecutables
                                                  )
                                                )
import qualified Agda.Parser                   as Parser
import           Agda.Position                  ( makeToOffset
                                                , toAgdaPositionWithoutFile
                                                )
import           Agda.Syntax.Abstract.Name      ( isLeParentModuleOf )
import           Agda.Syntax.Abstract.Pretty    ( prettyATop )
import           Agda.Syntax.Parser             ( exprParser
                                                , parse
                                                )
import           Agda.Syntax.Translation.ConcreteToAbstract
                                                ( concreteToAbstract_ )
import           Agda.Interaction.FindFile      ( findFile' )
#if MIN_VERSION_Agda(2,8,0)
#else
import           Agda.Interaction.FindFile      ( SourceFile(srcFilePath) )
#endif
import           Agda.Interaction.Library       ( OptionsPragma(pragmaStrings) )
import           Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName, projectRoot )
import           Agda.TypeChecking.Monad
import           Agda.Utils.FileName            ( AbsolutePath, absolute, doesFileExistCaseSensitive, filePath )
import           Agda.Utils.Hash                ( Hash, hashTextFile )
#if MIN_VERSION_Agda(2,8,0)
import           Agda.Syntax.Parser             ( agdaFileExtensions )
#else
import           Agda.Syntax.Parser             ( acceptableFileExts )
#endif
#if MIN_VERSION_Agda(2,8,0)
import           Agda.Interaction.Command       ( CommandM, localStateCommandM )
import           Agda.TypeChecking.Monad.Trace  ( runPM )
#endif
#if MIN_VERSION_Agda(2,8,0)
#else
import           Agda.TypeChecking.Warnings     ( runPM )
#endif
import           Agda.Syntax.Common.Pretty      ( render )
import           Control.Concurrent.STM
import           Control.Exception              ( IOException
                                                , SomeAsyncException
                                                , SomeException
                                                , catch
                                                , fromException
                                                , throwIO
                                                )
import           Control.Monad                  ( filterM, forM, forM_, unless )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable                  ( toList )
import           Data.IORef                     ( readIORef
                                                , writeIORef
                                                )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Language.LSP.Server            ( LspM )
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import qualified Language.LSP.VFS              as VFS
import           Monad
import           Options                        ( Config
                                                , Options(optNoHoverCache, optRawAgdaOptions)
                                                )
import           Data.Char                      ( toLower )
import           Data.List                      ( isSuffixOf )
import           System.Directory               ( listDirectory )
import           System.FilePath                ( (</>), takeDirectory )

initialiseCommandQueue :: IO CommandQueue
initialiseCommandQueue = CommandQueue <$> newTChanIO <*> newTVarIO Nothing

runCommandM :: CommandM a -> ServerM (LspM Config) (Either String a)
runCommandM program = do
  env <- ask
  runAgda $ do
    -- get command line options
    options <- getCommandLineOptions

    -- we need to set InteractionOutputCallback else it would panic
    lift $ setInteractionOutputCallback $ \_response -> return ()

    -- setup the command state
    commandQueue <- liftIO initialiseCommandQueue
    let commandState = (initCommandState commandQueue)
          { optionsOnReload = options { optAbsoluteIncludePaths = [] }
          }

    lift $ evalStateT program commandState

-- | The path Agda resolves a module to right now, if it resolves to one
resolveModule :: TopLevelModuleName -> TCM (Maybe AbsolutePath)
#if MIN_VERSION_Agda(2,8,0)
resolveModule m = findFile' m >>= either (const (return Nothing)) (fmap Just . srcFilePath)
#else
resolveModule m = either (const Nothing) (Just . srcFilePath) <$> findFile' m
#endif

hashOf :: AbsolutePath -> IO (Maybe Hash)
hashOf file = (Just <$> hashTextFile file) `catch` \(_ :: IOException) -> return Nothing

-- | The file extensions of Agda source files
sourceExtensions :: [String]
#if MIN_VERSION_Agda(2,8,0)
sourceExtensions = agdaFileExtensions
#else
sourceExtensions = acceptableFileExts
#endif

-- | The files that a module could be in, in these include dirs
candidateFiles :: [AbsolutePath] -> TopLevelModuleName -> [FilePath]
candidateFiles includes m = Set.toList $ Set.fromList
  [ filePath dir </> moduleNameToFileName m e | dir <- includes, e <- sourceExtensions ]

-- | A check of whether a module still has exactly one candidate file, and it
-- is this one, for these modules. It lists each directory that their
-- candidates are in once, and skips an include dir where that directory has
-- no file with an Agda extension, so that a module costs file checks only
-- in the include dirs that could have it. A directory that cannot be listed
-- is not skipped.
lookupCheck :: [AbsolutePath] -> [TopLevelModuleName] -> IO (TopLevelModuleName -> AbsolutePath -> IO Bool)
lookupCheck includes names = do
  let subdirs = Set.toList $ Set.fromList $ map subdirOf names
  hasSources <- fmap Map.fromList $ forM [ (dir, sub) | dir <- includes, sub <- subdirs ] $ \key@(dir, sub) -> do
    found <- (any isSourceFile <$> listDirectory (filePath dir </> sub)) `catch` \(_ :: IOException) -> return True
    return (key, found)
  return $ \m path -> do
    let dirs = [ dir | dir <- includes, Map.findWithDefault True (dir, subdirOf m) hasSources ]
    existing <- filterM doesFileExistCaseSensitive (candidateFiles dirs m)
    case existing of
      [file] | file == filePath path -> return True
             | otherwise -> (== path) <$> absolute file
      _ -> return False
  where
    subdirOf m = takeDirectory (moduleNameToFileName m "")
    -- in lower case, so that a file system that ignores case cannot hide a
    -- candidate
    isSourceFile name = any (`isSuffixOf` map toLower name) sourceExtensions

-- | Builds the check for later hovers from the load that just finished, or
-- gives Nothing if that load cannot be checked. The check runs Agda's own
-- library lookup and source hash, and replaces 'findFile'' with an existence
-- check over the same candidate paths.
loadCheck :: AbsolutePath -> CommandLineOptions -> TCM (Maybe (TCM Bool))
loadCheck mainFile opts = do
  visited <- Map.toList <$> getVisitedModules
  resolved <- forM visited $ \(m, mi) -> do
    path <- resolveModule m
    return $ (\p -> (m, p, iSourceHash (miInterface mi))) <$> path
  includes <- toList <$> getIncludeDirs
  case sequence resolved of
    Nothing -> return Nothing
    Just entries -> case [ m | (m, p, _) <- entries, p == mainFile ] of
      [] -> return Nothing
      mainModule : _ -> do
        let projects = Map.elems $ Map.fromList
              [ ((takeDirectory (filePath p), filePath (projectRoot p m)), (m, p)) | (m, p, _) <- entries ]
            libraryOptions = forM projects $ \(m, p) -> map pragmaStrings <$> getLibraryOptions p m
            sources = [ p | (_, p, _) <- entries ]
            hashes = [ Just h | (_, _, h) <- entries ]
        libOptions <- libraryOptions
        executables <- optTrustedExecutables <$> addTrustedExecutables opts
        return $ Just $ do
          -- cheapest check that finds an edit comes first
          hashes' <- liftIO $ mapM hashOf sources
          if hashes' /= hashes then return False else do
            setCommandLineOptions' (projectRoot mainFile mainModule) opts
            includes' <- toList <$> getIncludeDirs
            executables' <- optTrustedExecutables <$> addTrustedExecutables opts
            if includes' /= includes || executables' /= executables then return False else do
              -- the load proved that each module had exactly one candidate,
              -- the one Agda resolved, so only a later hover needs to look at
              -- the disk
              resolves <- liftIO $ do
                stillResolves <- lookupCheck includes [ m | (m, _, _) <- entries ]
                and <$> mapM (\(m, p, _) -> stillResolves m p) entries
              if not resolves then return False else (== libOptions) <$> libraryOptions

-- | The persistent state of the last load, for the next load to start from,
-- so that Agda reuses the modules it decoded. It drops each module whose
-- lookup changed, and each module that imports one of those, directly or
-- not. Agda checks the imports of a decoded module before it checks any
-- declaration, so a kept module reports a broken import with a shorter
-- range and less context than a fresh load does. Gives Nothing if the
-- lookups cannot be checked.
--
-- Agda's own 'modulesToKeep', in 'setIncludeDirs', does not cover this. It
-- drops the decoded modules whose lookup differs between the old and the
-- new include dirs, but 'cmd_load'' resets the state before it sets the
-- include dirs, which clears the old lookups, so Agda looks up the "old"
-- side on the disk as it is now. A change on the disk under the same
-- include dirs then goes unseen: when A/M.agda is deleted and B/M.agda is
-- added, Agda keeps the decoded M, and this function drops it.
persistentStateForReload :: TCState -> TCM (Maybe PersistentTCState)
persistentStateForReload old = do
  fresh <- getTC
  result <- (putTC old >> Just <$> keep) `orOnAnyException` return Nothing
  putTC fresh
  return result
  where
    persistent = stPersistentState old
    decoded = stDecodedModules persistent
    importsOf = map fst . iImportedModules . miInterface
    keep = do
      -- the include dirs of the last load, which its lookups used
      includes <- toList <$> getIncludeDirs
      let names = Set.toList $ Set.fromList $ Map.keys decoded ++ concatMap importsOf (Map.elems decoded)
      resolvesTo <- liftIO $ lookupCheck includes names
      let sameLookup m = resolveModule m >>= maybe (return False) (liftIO . resolvesTo m)
      changed <- Set.fromList <$> filterM (fmap not . sameLookup) names
      let addImporters bad = Set.union bad $ Map.keysSet $ Map.filter (any (`Set.member` bad) . importsOf) decoded
          closure bad = let bad' = addImporters bad in if bad' == bad then bad else closure bad'
      return persistent { stDecodedModules = Map.withoutKeys decoded (closure changed) }

-- | Runs a TCM action and gives the fallback on any exception, not only on
-- a 'TCErr': Agda reports a bug with '__IMPOSSIBLE__', which is a Haskell
-- exception, and the checks also do IO. Asynchronous exceptions, such as a
-- cancelled request, are thrown again.
orOnAnyException :: TCM a -> TCM a -> TCM a
orOnAnyException m fallback = TCM $ \r e ->
  unTCM m r e `catch` \(ex :: SomeException) -> case fromException ex of
    Just (async :: SomeAsyncException) -> throwIO async
    Nothing -> unTCM fallback r e

-- | Registers the current module and its submodules at the current
-- checkpoint, so that names defined in them are applied to the module
-- parameters. Runs inside 'atTopLevel'.
--
-- This works around Agda #8905. 'atTopLevel' moves the module checkpoints
-- that exist to the checkpoint of the context it builds, but after the
-- current module is loaded from its .agdai, none exist, so 'D' in
-- @module M (A : Set)@ comes out as @(A₁ : Set) → Set@ instead of @Set@.
-- The fix in Agda, agda/agda#8906, does the same as this function inside
-- 'atTopLevel', and is not merged.
--
-- When an Agda release has the fix, run this function only on the Agda
-- versions before that release, with a CPP version check, and remove it
-- once every supported Agda has the fix.
--
-- #8906 leaves open whether submodules with their own parameters should
-- get the checkpoint (they do here, as after a load that checks the
-- module), and whether loading the .agdai should register it instead. If
-- Agda decides otherwise, this function differs from Agda's fix on the
-- versions it still runs on. No test covers a submodule with parameters.
keepModuleParameters :: TCM ()
keepModuleParameters = do
  current <- currentModule
  sections <- Map.union <$> useTC (stSignature . sigSections)
                        <*> useTC (stImports . sigSections)
  forM_ (filter (current `isLeParentModuleOf`) (Map.keys sections)) setModuleCheckpoint

inferTypeOfText
  :: FilePath -> Text -> ServerM (LspM Config) (Either String String)
inferTypeOfText filepath text = do
  cacheRef <- asks envHoverCache
  -- with --no-hover-cache, every hover loads from a fresh state, and no
  -- cache is read, checked or stored
  noCache <- asks (optNoHoverCache . envOptions)
  runCommandM $ do
    cached <- if noCache then return Nothing else liftIO $ readIORef cacheRef
    -- ask Agda if it would still see the same inputs, and leave no trace of asking
    reusable <- case cached of
      Just c | hcFilePath c == filepath -> lift $ do
        fresh <- getTC
        ok <- hcStillValid c `orOnAnyException` return False
        putTC fresh
        return ok
      _ -> return False
    case cached of
      Just c | reusable -> do
        lift $ putTC (hcTCState c)
        put (hcCommandState c)
      _ -> do
        -- start from the modules the last load decoded, except the changed ones
        forM_ cached $ \c -> lift $ do
          kept <- persistentStateForReload (hcTCState c)
          forM_ kept $ \p -> modifyTC $ \st -> st { stPersistentState = p }
        -- load first
        cmd_load' filepath [] True Imp.TypeCheck $ \_ -> return ()
        unless noCache $ do
          tcState <- lift getTC
          commandState <- get
          mainFile <- liftIO $ absolute filepath
          opts <- gets optionsOnReload
          check <- lift $ loadCheck mainFile opts `orOnAnyException` return Nothing
          forM_ check $ \stillValid ->
            liftIO $ writeIORef cacheRef $ Just $
              HoverCache filepath stillValid tcState commandState
    -- infer later
    let norm = AsIs
    -- localStateCommandM: restore TC state afterwards, do we need this here?
    localStateCommandM $ do
      (e, _attrs) <- lift $ runPM $ parse exprParser (unpack text)
      -- print inside 'atTopLevel', which sets the scope of the module, like
      -- Agda's own Emacs mode does. The scope that 'cmd_load'' leaves is
      -- complete only when it checks the module, not when it loads the
      -- module from its .agdai, and printing in it qualifies names.
      lift $ atTopLevel $ do
        keepModuleParameters
        typ <- concreteToAbstract_ e >>= typeInCurrent norm
        render <$> prettyATop typ

onHover :: LSP.Uri -> LSP.Position -> ServerM (LspM Config) (LSP.Hover LSP.|? LSP.Null)
onHover uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing   -> return $ LSP.InR LSP.Null
    Just file -> do
      let source      = VFS.virtualFileText file
      let offsetTable = makeToOffset source
      let agdaPos     = toAgdaPositionWithoutFile offsetTable pos
      lookupResult <- Parser.tokenAt uri source agdaPos
      case lookupResult of
        Nothing             -> return $ LSP.InR LSP.Null
        Just (_token, text) -> do
          case LSP.uriToFilePath uri of
            Nothing       -> return $ LSP.InR LSP.Null
            Just filepath -> do
              let range = LSP.Range pos pos
              inferResult <- inferTypeOfText filepath text
              case inferResult of
                Left err -> do
                  let content = hoverContent $ "Error: " <> pack err
                  return $ LSP.InL $ LSP.Hover content (Just range)
                Right typeString -> do
                  let content = hoverContent $ pack typeString
                  return $ LSP.InL $ LSP.Hover content (Just range)
  where
      hoverContent =
        LSP.InL . LSP.mkMarkdownCodeBlock "agda-language-server"
--------------------------------------------------------------------------------
-- Helper functions for converting stuff to SemanticTokenAbsolute


fromHighlightingInfo :: IR.HighlightingInfo -> LSP.SemanticTokenAbsolute
fromHighlightingInfo (IR.HighlightingInfo start end aspects isTokenBased note defSrc)
  = LSP.SemanticTokenAbsolute 1 1 3 kw []
  where
    kw = LSP.SemanticTokenTypes_Keyword

-- HighlightingInfo
--       Int -- starting offset
--       Int -- ending offset
--       [String] -- list of names of aspects
--       Bool -- is token based?
--       String -- note
--       (Maybe (FilePath, Int)) -- the defining module of the token and its position in that module

-- toToken
--   :: Ranged a
--   => J.SemanticTokenTypes
--   -> [J.SemanticTokenModifiers]
--   -> a
--   -> [J.SemanticTokenAbsolute]
-- toToken types modifiers x =
--   let range = rangeOf x
--   in  [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
--                                 (posCol (rangeStart range) - 1)
--                                 (rangeSpan range)
--                                 types
--                                 modifiers
--       ]
