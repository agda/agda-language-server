{-# LANGUAGE CPP #-}

module Agda.Parser where

--------------------------------------------------------------------------------

#if MIN_VERSION_Agda(2,9,0)
import Agda (parseAgdaOptions)
import Agda.Interaction.Options (CommandLineOptions (optMdOnlyAgdaBlocks))
#endif
import Agda.Syntax.Parser (parseFile, runPMIO, tokensParser)
import Agda.Syntax.Parser.Tokens (Token (TokMarkup, TokTeX))
import Agda.Syntax.Position (Position' (posPos), PositionWithoutFile, Range, RangeFile (RangeFile), getRange, rEnd', rStart')
import Agda.Utils.FileName (mkAbsolute)
import Control.Monad.State
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server (LspM)
import Monad (ServerM)
import Options (Config)

--------------------------------------------------------------------------------

tokenAt :: LSP.Uri -> Text -> PositionWithoutFile -> ServerM (LspM Config) (Maybe (Token, Text))
tokenAt uri source position = case LSP.uriToFilePath uri of
  Nothing -> return Nothing
  Just filepath -> do
    let file = RangeFile (mkAbsolute filepath) Nothing
#if MIN_VERSION_Agda(2,9,0)
    -- read literate Markdown and Typst files as Agda does, which only
    -- treats '```agda' blocks as code with this option. When the options
    -- do not parse, Agda's default (False) applies, as in
    -- 'getCommandLineOptions'.
    mdOnlyAgdaBlocks <- maybe False (optMdOnlyAgdaBlocks . snd) <$> parseAgdaOptions
#endif
    (result, _warnings) <- liftIO $
      runPMIO $ do
        -- parse the file and get all tokens
#if MIN_VERSION_Agda(2,9,0)
        (r, _fileType) <- parseFile mdOnlyAgdaBlocks tokensParser file (unpack source)
#else
        (r, _fileType) <- parseFile tokensParser file (unpack source)
#endif
        let tokens = fst r
        -- find the token at the position
        return $ find (pointedBy position) tokens >>= nonLiterate
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
    -- the text of a literate file (prose, and the fences of its code
    -- blocks) is not code, so there is nothing to infer
    nonLiterate :: Token -> Maybe Token
    nonLiterate (TokTeX _) = Nothing
    nonLiterate (TokMarkup _) = Nothing
    nonLiterate token = Just token

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
