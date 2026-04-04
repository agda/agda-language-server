module Server.Model.AgdaLib
  ( AgdaLibOrigin (..),
    AgdaLib (AgdaLib),
    agdaLibName,
    agdaLibIncludes,
    agdaLibDependencies,
    agdaLibFromFile,
    agdaLibToFile,
  )
where

import Agda.Interaction.Library (AgdaLibFile (AgdaLibFile), LibName, OptionsPragma)
import Agda.Interaction.Library.Base (libDepends, libIncludes, libName, libPragmas)
import Agda.Syntax.Common.Pretty (Pretty, doubleQuotes, pretty, pshow, text, (<+>))
import Agda.Utils.Lens (Lens', (<&>), (^.))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.Uri.More (uriToPossiblyInvalidFilePath)
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import qualified Server.Filesystem as FS

data AgdaLibOrigin = FromFile !FS.FileId | Defaulted
  deriving (Show, Eq)

data AgdaLib = AgdaLib
  { _agdaLibName :: !LibName,
    _agdaLibIncludes :: ![FS.FileId],
    _agdaLibOptionsPragma :: !OptionsPragma,
    _agdaLibDependencies :: ![LibName],
    _agdaLibFile :: !FS.FileId
  }

instance Pretty AgdaLib where
  pretty agdaLib =
    text "AgdaLib"
      <+> doubleQuotes (pretty $ agdaLib ^. agdaLibName)
      <+> text "file:"
      <+> pshow (agdaLib ^. agdaLibFile)
      <+> text "includes:"
      <+> pretty (agdaLib ^. agdaLibIncludes)
      <+> text "options:"
      <+> pshow (agdaLib ^. agdaLibOptionsPragma)
      <+> text "dependencies:"
      <+> pretty (agdaLib ^. agdaLibDependencies)

agdaLibName :: Lens' AgdaLib LibName
agdaLibName f a = f (_agdaLibName a) <&> \x -> a {_agdaLibName = x}

agdaLibIncludes :: Lens' AgdaLib [FS.FileId]
agdaLibIncludes f a = f (_agdaLibIncludes a) <&> \x -> a {_agdaLibIncludes = x}

agdaLibOptionsPragma :: Lens' AgdaLib OptionsPragma
agdaLibOptionsPragma f a = f (_agdaLibOptionsPragma a) <&> \x -> a {_agdaLibOptionsPragma = x}

agdaLibDependencies :: Lens' AgdaLib [LibName]
agdaLibDependencies f a = f (_agdaLibDependencies a) <&> \x -> a {_agdaLibDependencies = x}

agdaLibFile :: Lens' AgdaLib FS.FileId
agdaLibFile f a = f (_agdaLibFile a) <&> \x -> a {_agdaLibFile = x}

-- | Given an 'AgdaLibFile' and the URI of that file, create the
-- corresponding 'AgdaLib'
agdaLibFromFile :: (MonadIO m, FS.IsFileId f) => AgdaLibFile -> f -> m AgdaLib
agdaLibFromFile agdaLibFile agdaLibIsFileId = do
  let agdaLibFileId = FS.toFileId agdaLibIsFileId
  agdaLibParent <- FS.fileIdParent agdaLibFileId
  let includeToAbsolute = case agdaLibParent of
        Nothing -> return . FS.LocalFilePath
        Just parent -> \include -> FS.LocalFilePath include `FS.fileIdRelativeTo` parent
  includes <- forM (agdaLibFile ^. libIncludes) includeToAbsolute
  return $
    AgdaLib
      (agdaLibFile ^. libName)
      includes
      (agdaLibFile ^. libPragmas)
      (agdaLibFile ^. libDepends)
      agdaLibFileId

-- | Turn an `AgdaLib` back into a file. Since `AgdaLibFile`s are relative to an
-- Agda file on the filesystem, the first parameter is for the URI for the Agda
-- file
agdaLibToFile :: LSP.NormalizedUri -> AgdaLib -> AgdaLibFile
agdaLibToFile relativeToUri agdaLib =
  let includePaths = uriToPossiblyInvalidFilePath . FS.fileIdToUri <$> agdaLib ^. agdaLibIncludes
      uri = FS.fileIdToUri $ agdaLib ^. agdaLibFile
      above = LSP.uriHeightAbove uri relativeToUri
      filePath = LSP.uriToPossiblyInvalidFilePath uri
   in AgdaLibFile
        (agdaLib ^. agdaLibName)
        filePath
        above
        includePaths
        (agdaLib ^. agdaLibDependencies)
        (agdaLib ^. agdaLibOptionsPragma)
