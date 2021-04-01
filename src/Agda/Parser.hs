module Agda.Parser where

--------------------------------------------------------------------------------

import Agda.Syntax.Parser (parseFile, runPMIO, tokensParser)
import Agda.Syntax.Parser.Tokens (Token)
import Agda.Syntax.Position (Position' (posPos), PositionWithoutFile, Range, getRange, rEnd', rStart')
import Agda.Utils.FileName (mkAbsolute)
import Common
import Control.Monad.State
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Language.LSP.Server (LspM)
import qualified Language.LSP.Types as LSP

--------------------------------------------------------------------------------

tokenAt :: LSP.Uri -> Text -> PositionWithoutFile -> ServerM (LspM ()) (Maybe (Token, Text))
tokenAt uri source position = case LSP.uriToFilePath uri of
  Nothing -> return Nothing
  Just filepath -> do
    (result, _warnings) <- liftIO $
      runPMIO $ do
        -- parse the file and get all tokens
        (tokens, _fileType) <- parseFile tokensParser (mkAbsolute filepath) (unpack source)
        -- find the token at the position
        return $ find (pointedBy position) tokens
    case result of
      Left _err -> return Nothing
      Right Nothing -> return Nothing
      Right (Just token) -> do
        -- get the range of the token
        case tokenOffsets token of
          Nothing -> return Nothing
          Just (start, end) -> do
            -- get the text from the range of the token
            let text = Text.drop (start - 1) (Text.take (end - 1) source)
            return $ Just (token, text)
  where
    startAndEnd :: Range -> Maybe (PositionWithoutFile, PositionWithoutFile)
    startAndEnd range = do
      start <- rStart' range
      end <- rEnd' range
      return (start, end)

    pointedBy :: PositionWithoutFile -> Token -> Bool
    pointedBy pos token = fromMaybe False $ do
      (start, end) <- startAndEnd (getRange token)
      return $ start <= pos && pos < end

    tokenOffsets :: Token -> Maybe (Int, Int)
    tokenOffsets token = do
      (start, end) <- startAndEnd (getRange token)
      return (fromIntegral (posPos start), fromIntegral (posPos end))