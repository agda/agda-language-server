{-# LANGUAGE DataKinds #-}

module Server.VfsIndex
  ( Entry,
    getEntryChildren,
    VfsIndex,
    empty,
    onOpen,
    onClose,
    getEntry,
  )
where

import Agda.Utils.Functor ((<&>))
import Agda.Utils.List1 (NonEmpty ((:|)))
import Agda.Utils.Maybe (maybeToList)
import Data.List (delete, union)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP
import qualified Text.URI as ParsedUri

data Entry = Entry
  { _children :: ![ParsedUri.URI],
    _isInVfs :: !Bool
  }

instance Semigroup Entry where
  (Entry childrenA isInVfsA) <> (Entry childrenB isInVfsB) = Entry (union childrenA childrenB) (isInVfsA || isInVfsB)

getEntryChildren :: Entry -> [LSP.NormalizedUri]
getEntryChildren = fmap (LSP.toNormalizedUri . LSP.Uri . ParsedUri.render) . _children

-- | The index allows users to traverse the VFS like a tree. We infer an entry
-- for each "directory" which ultimately contains actual VFS entries.
-- For example, say the VFS contains files with the following URIs:
-- - @file:\/\/\/a\/b\/c.agda@
-- - @file:\/\/\/a\/.agda-lib@
--
-- The index would store entries for the following URIs:
-- - @file:\/\/@
-- - @file:\/\/\/a@
-- - @file:\/\/\/a\/b@
-- - @file:\/\/\/a\/b\/c.agda@
-- - @file:\/\/\/a\/.agda-lib@
data VfsIndex = VfsIndex (Map ParsedUri.URI Entry)

empty :: VfsIndex
empty = VfsIndex Map.empty

onOpen :: LSP.Uri -> VfsIndex -> VfsIndex
onOpen uri vfsIndex =
  case ParsedUri.mkURI $ LSP.getNormalizedUri $ LSP.toNormalizedUri uri of
    Nothing -> vfsIndex -- TODO: log this as an error
    Just uri ->
      let entryUris = dice uri
          entries =
            zip entryUris (True : repeat False)
              <&> \(uri, isInVfs) -> (uri, Entry (maybeToList $ chop uri) isInVfs)
       in foldl' (\vfsIndex (uri, entry) -> insertEntry uri entry vfsIndex) vfsIndex entries

onClose :: LSP.Uri -> VfsIndex -> VfsIndex
onClose uri vfsIndex =
  case ParsedUri.mkURI $ LSP.getNormalizedUri $ LSP.toNormalizedUri uri of
    Nothing -> vfsIndex -- TODO: log this as an error
    Just uri -> removeEntryFromVfs uri vfsIndex

getEntry :: LSP.NormalizedUri -> VfsIndex -> Maybe Entry
getEntry uri (VfsIndex map) =
  case ParsedUri.mkURI $ LSP.getNormalizedUri uri of
    Nothing -> Nothing
    Just uri -> Map.lookup uri map

insertEntry :: ParsedUri.URI -> Entry -> VfsIndex -> VfsIndex
insertEntry uri entry (VfsIndex map) = VfsIndex $ Map.insertWith (<>) uri entry map

removeEntryFromVfs :: ParsedUri.URI -> VfsIndex -> VfsIndex
removeEntryFromVfs uri (VfsIndex map) = deleteIfInvalidLeaf uri $ VfsIndex $ Map.adjust (\entry -> entry {_isInVfs = False}) uri map

updateParentAfterDelete :: ParsedUri.URI -> VfsIndex -> VfsIndex
updateParentAfterDelete childUri vfsIndex@(VfsIndex map) =
  case chop childUri of
    Nothing -> vfsIndex
    Just parentUri ->
      deleteIfInvalidLeaf parentUri $
        VfsIndex $
          Map.adjust (\entry -> entry {_children = delete childUri $ _children entry}) parentUri map

isValidLeaf :: Entry -> Bool
isValidLeaf entry = _isInVfs entry || not (null (_children entry))

deleteIfInvalidLeaf :: ParsedUri.URI -> VfsIndex -> VfsIndex
deleteIfInvalidLeaf uri vfsIndex@(VfsIndex map) =
  case Map.lookup uri map of
    Just entry
      | not (isValidLeaf entry) ->
          updateParentAfterDelete uri $ VfsIndex $ Map.delete uri map
    _ -> vfsIndex

dice :: ParsedUri.URI -> [ParsedUri.URI]
dice uri = uri : concat (dice <$> chop uri)

chop :: ParsedUri.URI -> Maybe ParsedUri.URI
chop (ParsedUri.URI scheme authority path query (Just _fragment)) =
  Just $ ParsedUri.URI scheme authority path query Nothing
chop (ParsedUri.URI scheme authority path query@(_ : _) Nothing) =
  Just $ ParsedUri.URI scheme authority path (init query) Nothing
chop (ParsedUri.URI scheme authority path@(Just _) [] Nothing) =
  Just $ ParsedUri.URI scheme authority (chopPath path) [] Nothing
chop (ParsedUri.URI scheme authority@(Right _) Nothing [] Nothing) =
  Just $ ParsedUri.URI scheme (Left True) Nothing [] Nothing
chop (ParsedUri.URI scheme@(Just _) authority@(Left _) Nothing [] Nothing) =
  Just $ ParsedUri.URI Nothing authority Nothing [] Nothing
chop (ParsedUri.URI Nothing authority@(Left _) Nothing [] Nothing) = Nothing

type Path = Maybe (Bool, NonEmpty (ParsedUri.RText 'ParsedUri.PathPiece))

chopPath :: Path -> Path
chopPath (Just (True, parts)) = Just (False, parts)
chopPath (Just (False, part :| parts@(_ : _))) = Just (False, part :| init parts)
chopPath (Just (False, part :| [])) = Nothing
chopPath Nothing = Nothing
