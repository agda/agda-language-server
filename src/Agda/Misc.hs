{-# LANGUAGE OverloadedStrings #-}

module Agda.Misc where

import Agda.Interaction.BasicOps (typeInCurrent, parseExpr)
import Agda.Interaction.InteractionTop (parseAndDoAtToplevel)
import qualified Agda.Parser as Parser
import Agda.Position (makeOffsetTable, toPositionWithoutFile)
-- import Agda.Syntax.Position (Range' (Range))
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS
import Common (ServerM, ServerM')
import Agda (runTCMPrettyErrors)
import Agda.TypeChecking.Monad (TCM)
import Data.Text (Text, unpack, pack)
import Control.Monad.State 
import Control.Monad.Reader
import Agda.Interaction.Base (CommandState(optionsOnReload), initCommandState, Rewrite (AsIs))
import Agda.Interaction.Options (CommandLineOptions(optAbsoluteIncludePaths))
import Agda.Syntax.Position (Range, getRange)
import Agda.Syntax.Translation.ConcreteToAbstract (concreteToAbstract_)
import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Utils.Pretty (render)

-- inferType :: Text -> LSP.LspT () ServerM (Maybe LSP.Hover)

inferTypeOfText :: Range -> Text -> LSP.LspT () ServerM (Either String String)
inferTypeOfText range text = lift $ runTCMPrettyErrors $ do 
  lift $ do 
    expr <- parseExpr range (unpack text)
    expr' <- concreteToAbstract_ expr
    typ <- typeInCurrent AsIs expr' 
    render <$> prettyATop typ 



onHover :: LSP.Uri -> LSP.Position -> LSP.LspT () ServerM (Maybe LSP.Hover)
onHover uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing -> return Nothing
    Just file -> do
      let source = VFS.virtualFileText file
      let offsetTable = makeOffsetTable source
      let agdaPos = toPositionWithoutFile offsetTable pos
      lookupResult <- Parser.tokenAt uri source agdaPos
      case lookupResult of
        Nothing -> return Nothing
        Just (token, text) -> do
          let range = LSP.Range pos pos

          inferResult <- inferTypeOfText (getRange token) text 
          case inferResult of 
            Left error -> do 
              let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" ("Error: " <> pack error)
              return $ Just $ LSP.Hover content (Just range)
            Right typeString -> do 
              let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" ("Error: " <> pack typeString)
              return $ Just $ LSP.Hover content (Just range)
