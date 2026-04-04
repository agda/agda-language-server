module Server.Model.InstalledLibsFile
  ( InstalledLibsFile,
    entries,
    fromFileId,
    Entry,
    entryLineNumber,
    entryLibFileId,
  )
where

import Agda.Interaction.Library.Base (LineNumber)
import Agda.Interaction.Library.Parse.More (trimLineComment)
import Agda.Utils.Environment (expandEnvironmentVariables)
import Agda.Utils.Lens (Lens', (<&>))
import Agda.Utils.Maybe (caseMaybeM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Server.Filesystem as FS

data InstalledLibsFile = InstalledLibsFile
  { _fileId :: !FS.FileId,
    _libFileEntries :: ![Entry]
  }

fileId :: Lens' InstalledLibsFile FS.FileId
fileId f a = f (_fileId a) <&> \x -> a {_fileId = x}

entries :: Lens' InstalledLibsFile [Entry]
entries f a = f (_libFileEntries a) <&> \x -> a {_libFileEntries = x}

data Entry = Entry
  { _lineNumber :: !LineNumber,
    _libFileId :: !FS.FileId
  }

entryLineNumber :: Lens' Entry LineNumber
entryLineNumber f a = f (_lineNumber a) <&> \x -> a {_lineNumber = x}

entryLibFileId :: Lens' Entry FS.FileId
entryLibFileId f a = f (_libFileId a) <&> \x -> a {_libFileId = x}

fromFileId :: (FS.MonadFilesystem m, FS.Provider p) => p -> FS.FileId -> m (Maybe InstalledLibsFile)
fromFileId provider fileId =
  caseMaybeM
    (FS.getFileContents provider fileId)
    (return Nothing)
    $ \contents ->
      Just . InstalledLibsFile fileId <$> parse contents

parse :: (MonadIO m) => Text -> m [Entry]
parse contents = do
  let strContents = Text.unpack contents
  let lines = stripCommentLines strContents
  liftIO $ sequence [Entry i . FS.LocalFilePath <$> expandEnvironmentVariables s | (i, s) <- lines]

-- | Remove trailing white space and line comments.
stripCommentLines :: String -> [(LineNumber, String)]
stripCommentLines = concatMap strip . zip [1 ..] . lines
  where
    strip (i, s) = [(i, s') | not $ null s']
      where
        s' = trimLineComment s
