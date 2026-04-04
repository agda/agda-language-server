{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Adaptation of Agda.Interaction.Library.Parse
--
-- Agda only exports functions which read from the filesystem, so we reproduce
-- and modify them here to interact with LSP filesystem abstractions.
module Agda.Interaction.Library.Parse.More
  ( parseLibFile,
    runP,
    trimLineComment,
  )
where

import Agda.Interaction.Library.Base
import Agda.Syntax.Position
import Agda.Utils.Applicative
import Agda.Utils.FileName
import Agda.Utils.Lens
import Agda.Utils.List (duplicates)
import qualified Agda.Utils.List1 as List1
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Utils.Singleton
import Agda.Utils.String (ltrim)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Char
import qualified Data.List as List
import qualified Data.Text as Text
import Monad (ServerM)
import qualified Server.Filesystem as FS

-- | Parser monad: Can throw @LibParseError@s, and collects
-- @LibWarning'@s library warnings.
type P = ExceptT LibParseError (Writer [LibWarning'])

runP :: P a -> (Either LibParseError a, [LibWarning'])
runP = runWriter . runExceptT

warningP :: LibWarning' -> P ()
warningP = tell . pure

-- | The config files we parse have the generic structure of a sequence
--   of @field : content@ entries.
type GenericFile = [GenericEntry]

data GenericEntry = GenericEntry
  { -- | E.g. field name.    @trim@med.
    geHeader :: String,
    -- | E.g. field content. @trim@med.
    _geContent :: [String]
  }

-- | Library file field format format [sic!].
data Field = forall a. Field
  { -- | Name of the field.
    fName :: String,
    -- | Is it optional?
    fOptional :: Bool,
    -- | Content parser for this field.
    --
    -- The range points to the start of the file.
    fParse :: Range -> [String] -> P a,
    -- | Sets parsed content in 'AgdaLibFile' structure.
    fSet :: LensSet AgdaLibFile a
  }

optionalField ::
  String -> (Range -> [String] -> P a) -> Lens' AgdaLibFile a -> Field
optionalField str p l = Field str True p (set l)

-- | @.agda-lib@ file format with parsers and setters.
agdaLibFields :: [Field]
agdaLibFields =
  -- Andreas, 2017-08-23, issue #2708, field "name" is optional.
  [ optionalField "name" (\_ -> parseName) libName,
    optionalField "include" (\_ -> pure . concatMap parsePaths) libIncludes,
#if MIN_VERSION_Agda(2,8,0)
    optionalField "depend" (\_ -> pure . map parseLibName . concatMap splitCommas) libDepends,
#else
    optionalField "depend" (\_ -> pure . concatMap splitCommas) libDepends,
#endif
    optionalField "flags" (\r -> pure . foldMap (parseFlags r)) libPragmas
  ]
  where
    parseName :: [String] -> P LibName
#if MIN_VERSION_Agda(2,8,0)
    parseName [s] | [name] <- words s = pure $ parseLibName name
#else
    parseName [s] | [name] <- words s = pure name
#endif
    parseName ls = throwError $ BadLibraryName $ unwords ls

    parsePaths :: String -> [FilePath]
    parsePaths = go id
      where
        fixup acc = let fp = acc [] in not (null fp) ?$> fp
        go acc [] = fixup acc
        go acc ('\\' : ' ' : cs) = go (acc . (' ' :)) cs
        go acc ('\\' : '\\' : cs) = go (acc . ('\\' :)) cs
        go acc (' ' : cs) = fixup acc ++ go id cs
        go acc (c : cs) = go (acc . (c :)) cs

    parseFlags :: Range -> String -> OptionsPragma
    parseFlags r s =
      OptionsPragma
        { pragmaStrings = words s,
          pragmaRange = r
        }

-- | Parse @.agda-lib@ file.
--
-- In the Agda implementation, this is where the path is set in AgdaLibFile. We
-- don't, since we don't really want to work with paths in the first place.
parseLibFile :: (FS.Provider p, FS.IsFileId f) => p -> f -> ServerM (Maybe (P AgdaLibFile))
parseLibFile provider fileId = do
  abs <- FS.fileIdToPossiblyInvalidAbsolutePath $ FS.toFileId fileId
  contents <- FS.getFileContents provider fileId
  case contents of
    Nothing -> return Nothing
    Just contents ->
      return $ Just $ parseLib abs (Text.unpack contents)

-- | Parse file contents.
parseLib ::
  -- | The parsed file.
  AbsolutePath ->
  String ->
  P AgdaLibFile
parseLib file s = fromGeneric file =<< parseGeneric s

-- | Parse 'GenericFile' with 'agdaLibFields' descriptors.
fromGeneric ::
  -- | The parsed file.
  AbsolutePath ->
  GenericFile ->
  P AgdaLibFile
fromGeneric file = fromGeneric' file agdaLibFields

-- | Given a list of 'Field' descriptors (with their custom parsers),
--   parse a 'GenericFile' into the 'AgdaLibFile' structure.
--
--   Checks mandatory fields are present;
--   no duplicate fields, no unknown fields.
fromGeneric' ::
  -- | The parsed file.
  AbsolutePath ->
  [Field] ->
  GenericFile ->
  P AgdaLibFile
fromGeneric' file fields fs = do
  checkFields fields (map geHeader fs)
  foldM upd emptyLibFile fs
  where
    -- The range points to the start of the file.
    r =
      Range
        (Strict.Just $ mkRangeFile file Nothing)
        (singleton (posToInterval () p p))
      where
        p =
          Pn
            { srcFile = (),
              posPos = 1,
              posLine = 1,
              posCol = 1
            }

    upd :: AgdaLibFile -> GenericEntry -> P AgdaLibFile
    upd l (GenericEntry h cs) = do
      mf <- findField h fields
      case mf of
        Just Field {..} -> do
          x <- fParse r cs
          return $ fSet x l
        Nothing -> return l

-- | Ensure that there are no duplicate fields and no mandatory fields are missing.
checkFields :: [Field] -> [String] -> P ()
checkFields fields fs = do
  -- Report missing mandatory fields.
  () <- List1.unlessNull missing $ throwError . MissingFields
  -- Report duplicate fields.
  List1.unlessNull (duplicates fs) $ throwError . DuplicateFields
  where
    mandatory :: [String]
    mandatory = [fName f | f <- fields, not $ fOptional f]
    missing :: [String]
    missing = mandatory List.\\ fs

-- | Find 'Field' with given 'fName', throw error if unknown.
findField :: String -> [Field] -> P (Maybe Field)
findField s fs = maybe err (return . Just) $ List.find ((s ==) . fName) fs
  where
    err = warningP (UnknownField s) >> return Nothing

-- Generic file parser ----------------------------------------------------

-- | Example:
--
-- @
--     parseGeneric "name:Main--BLA\ndepend:--BLA\n  standard-library--BLA\ninclude : . --BLA\n  src more-src   \n"
--     == Right [("name",["Main"]),("depend",["standard-library"]),("include",[".","src more-src"])]
-- @
parseGeneric :: String -> P GenericFile
parseGeneric s =
  groupLines =<< concat <$> zipWithM parseLine [1 ..] (map stripComments $ lines s)

-- | Lines with line numbers.
data GenericLine
  = -- | Header line, like a field name, e.g. "include :".  Cannot be indented.
    --   @String@ is 'trim'med.
    Header LineNumber String
  | -- | Other line.  Must be indented.
    --   @String@ is 'trim'med.
    Content LineNumber String
  deriving (Show)

-- | Parse line into 'Header' and 'Content' components.
--
--   Precondition: line comments and trailing whitespace have been stripped away.
--
--   Example file:
--
--   @
--     name: Main
--     depend:
--       standard-library
--     include: .
--       src more-src
--   @
--
--   This should give
--
--   @
--     [ Header  1 "name"
--     , Content 1 "Main"
--     , Header  2 "depend"
--     , Content 3 "standard-library"
--     , Header  4 "include"
--     , Content 4 "."
--     , Content 5 "src more-src"
--     ]
--   @
parseLine :: LineNumber -> String -> P [GenericLine]
parseLine _ "" = pure []
parseLine l s@(c : _)
  -- Indented lines are 'Content'.
  | isSpace c = pure [Content l $ ltrim s]
  -- Non-indented lines are 'Header'.
  | otherwise =
      case break (== ':') s of
        -- Headers are single words followed by a colon.
        -- Anything after the colon that is not whitespace is 'Content'.
        (h, ':' : r) ->
          case words h of
            [h] -> pure $ Header l h : [Content l r' | let r' = ltrim r, not (null r')]
            [] -> throwError $ MissingFieldName l
            hs -> throwError $ BadFieldName l h
        _ -> throwError $ MissingColonForField l (ltrim s)

-- | Collect 'Header' and subsequent 'Content's into 'GenericEntry'.
--
--   Leading 'Content's?  That's an error.
groupLines :: [GenericLine] -> P GenericFile
groupLines [] = pure []
groupLines (Content l c : _) = throwError $ ContentWithoutField l
groupLines (Header _ h : ls) = (GenericEntry h [c | Content _ c <- cs] :) <$> groupLines ls1
  where
    (cs, ls1) = span isContent ls
    isContent Content {} = True
    isContent Header {} = False

-- | Break a comma-separated string.  Result strings are @trim@med.
splitCommas :: String -> [String]
splitCommas = words . map (\c -> if c == ',' then ' ' else c)

-- | Remove leading whitespace and line comment.
trimLineComment :: String -> String
trimLineComment = stripComments . ltrim

-- | ...and trailing, but not leading, whitespace.
stripComments :: String -> String
stripComments "" = ""
stripComments ('-' : '-' : c : _) | isSpace c = ""
stripComments (c : s) = cons c (stripComments s)
  where
    cons c "" | isSpace c = ""
    cons c s = c : s