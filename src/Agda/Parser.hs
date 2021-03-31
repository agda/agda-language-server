-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Agda.Parser where

-- import Agda.Syntax.Position
-- import Agda.Utils.FileName (AbsolutePath (AbsolutePath))
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
-- import qualified Data.Strict.Maybe as Strict
-- import Data.Text (Text)
import qualified Language.LSP.Types as LSP

--------------------------------------------------------------------------------

import qualified Agda.Main as Agda
import Common
import Control.Monad.State
import Language.LSP.Server (LspT)
import Data.Text (Text, unpack)
import Agda.Syntax.Parser (parseFile, runPMIO, tokensParser, ParseError)
import Agda.Utils.FileName (mkAbsolute)
import Agda.Syntax.Position (getRange, rStart', rEnd', PositionWithoutFile, rangeToInterval, Range, Position' (posPos))
import Agda.Syntax.Parser.Tokens (Token)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text



--------------------------------------------------------------------------------

tokenAt :: LSP.Uri -> Text -> PositionWithoutFile -> LspT () ServerM (Maybe (Token, Text))
tokenAt uri source position = case LSP.uriToFilePath uri of
  Nothing -> return Nothing 
  Just filepath -> do
    (result, _warnings) <- liftIO $ runPMIO $ do
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