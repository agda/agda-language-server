{-# LANGUAGE CPP #-}

module Agda.Interaction.Library.More
  ( defaultLibraryFileIds,
    tryRunLibM,
#if MIN_VERSION_Agda(2,8,0)
#else
    runLibErrorIO,
#endif
  )
where

import Agda.Interaction.Library (LibM, AgdaLibFile)
import Agda.Interaction.Library.Base
  (
    libName,
    libFile,
    libIncludes,
#if MIN_VERSION_Agda(2,8,0)
#else
    LibErrorIO,
#endif
  )
import Agda.Utils.Either (maybeRight)
import Agda.Utils.Null (Null (empty))
import Control.Category ((>>>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Lazy (evalStateT)
import Control.Monad.Writer.Lazy (runWriterT)
import Agda.Syntax.Common.Pretty (Pretty, pretty, text, (<+>))
import Agda.Utils.Lens ((^.))
import Agda.Utils.List1 (List1, NonEmpty ((:|)))
import Agda.Version (version)
import qualified Server.Filesystem as FS

-- | The @~/.agda/libraries@ file lists the libraries Agda should know about.
--   The content of @libraries@ is a list of paths to @.agda-lib@ files.
--
--   Agda honors also version-specific @libraries@ files, e.g. @libraries-2.6.0@.
--
--   @defaultLibraryFiles@ gives a list of all @libraries@ files Agda should process
--   by default.  The first file in this list that exists is actually used.
--
defaultLibraryFiles :: List1 FilePath
defaultLibraryFiles = ("libraries-" ++ version) :| "libraries" : []

defaultLibraryFileIds :: (MonadIO m) => FS.FileId -> m (List1 FS.FileId)
defaultLibraryFileIds agdaDir =
  traverse (`FS.fileIdRelativeTo` agdaDir) . fmap FS.LocalFilePath $ defaultLibraryFiles

#if MIN_VERSION_Agda(2,8,0)
-- Unneeded in 2.8.0 due to API changes
#else
runLibErrorIO :: (MonadIO m) => LibErrorIO a -> m a
runLibErrorIO =
  runWriterT
    >>> flip evalStateT empty
    >>> fmap fst
    >>> liftIO
#endif

tryRunLibM :: (MonadIO m) => LibM a -> m (Maybe a)
tryRunLibM =
  runExceptT
    >>> runWriterT
    >>> flip evalStateT empty
    >>> fmap (fst >>> maybeRight)
    >>> liftIO

instance Pretty AgdaLibFile where
  pretty agdaLibFile =
    text "AgdaLibFile"
      <+> (pretty $ agdaLibFile ^. libName)
      <+> (pretty $ agdaLibFile ^. libFile)
      <+> (pretty $ agdaLibFile ^. libIncludes)
