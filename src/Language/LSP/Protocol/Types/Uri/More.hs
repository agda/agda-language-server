module Language.LSP.Protocol.Types.Uri.More
  ( getNormalizedUri,
    isUriAncestorOf,
    uriHeightAbove,
    uriParent,
    uriExtension,
    uriToPossiblyInvalidAbsolutePath,
    uriToPossiblyInvalidFilePath,
  )
where

import Agda.Utils.FileName (AbsolutePath (AbsolutePath), absolute)
import Agda.Utils.Lens (set, (^.))
import Agda.Utils.List (initMaybe, lastMaybe)
import Agda.Utils.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Types as LSP
import qualified Text.URI as ParsedUri
import qualified Text.URI.Lens as ParsedUriLens

getNormalizedUri :: LSP.NormalizedUri -> Text
getNormalizedUri = LSP.getUri . LSP.fromNormalizedUri

-- | Determine if the first URI is an ancestor of the second.
--
-- This is a heuristic implementation and may need replacement if the heuristic
-- leads to bugs.
isUriAncestorOf :: LSP.NormalizedUri -> LSP.NormalizedUri -> Bool
isUriAncestorOf ancestor descendant =
  getNormalizedUri ancestor `Text.isPrefixOf` getNormalizedUri descendant

-- | If @ancestor@ is an ancestor of @descendant@, then
-- @uriHeightAbove ancestor descendant@ is the height of @ancestor@ above
-- @descendant@.
--
-- For example, the height of @https://example.com/a/@ over
-- @https://example.com/a/b/c/@ is 2.
--
-- This is a heuristic implementation and may need replacement if the heuristic
-- leads to bugs.
uriHeightAbove :: LSP.NormalizedUri -> LSP.NormalizedUri -> Int
uriHeightAbove ancestor descendant =
  let suffix = pathBetween (getNormalizedUri ancestor) (getNormalizedUri descendant)
      path = stripSlash $ stripScheme suffix
   in countPathComponents path
  where
    pathBetween :: Text -> Text -> Text
    pathBetween a b = case Text.commonPrefixes a b of
      Just (_prefix, _suffixA, suffixB) -> suffixB
      Nothing -> ""

    stripScheme :: Text -> Text
    stripScheme uri =
      let (prefix, suffix) = Text.breakOn "//" uri
       in if Text.null suffix then prefix else suffix

    stripSlash :: Text -> Text
    stripSlash = Text.dropAround (== '/')

    countPathComponents :: Text -> Int
    countPathComponents path =
      if Text.null path
        then 0
        else Text.count "/" path + 1

uriParent :: LSP.NormalizedUri -> Maybe LSP.NormalizedUri
uriParent uri = do
  parsedUri <- ParsedUri.mkURI $ getNormalizedUri uri
  pathInit <- initMaybe $ parsedUri ^. ParsedUriLens.uriPath
  let newParsedUri = set ParsedUriLens.uriPath pathInit parsedUri
  return $ LSP.toNormalizedUri $ LSP.Uri $ ParsedUri.render newParsedUri

uriExtension :: LSP.NormalizedUri -> Text
uriExtension uri = fromMaybe "" $ do
  parsedUri <- ParsedUri.mkURI $ getNormalizedUri uri
  pathEndRefined <- lastMaybe $ parsedUri ^. ParsedUriLens.uriPath
  let pathEnd = ParsedUri.unRText pathEndRefined
  let (prefix, suffix) = Text.breakOnEnd "." pathEnd
  if Text.null prefix -- The prefix will contain the final ".", if one is found
    then Nothing
    else return $ "." <> suffix

uriToPossiblyInvalidAbsolutePath :: (MonadIO m) => LSP.NormalizedUri -> m AbsolutePath
uriToPossiblyInvalidAbsolutePath uri = do
  case LSP.uriToFilePath $ LSP.fromNormalizedUri uri of
    Just path -> liftIO $ absolute path
    Nothing -> return $ uriToInvalidAbsolutePath uri

uriToInvalidAbsolutePath :: LSP.NormalizedUri -> AbsolutePath
uriToInvalidAbsolutePath = AbsolutePath . getNormalizedUri

uriToPossiblyInvalidFilePath :: LSP.NormalizedUri -> FilePath
uriToPossiblyInvalidFilePath uri = case LSP.uriToFilePath $ LSP.fromNormalizedUri uri of
  Just path -> path
  Nothing -> uriToInvalidFilePath uri

uriToInvalidFilePath :: LSP.NormalizedUri -> FilePath
uriToInvalidFilePath = Text.unpack . getNormalizedUri
