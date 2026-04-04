module Server.Handler.TextDocument.DocumentSymbol (documentSymbolHandler) where

import qualified Agda.Syntax.Abstract as A
import Agda.Utils.Maybe (fromMaybe, mapMaybe)
import Agda.Utils.Monad (forMaybeM)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Monad (ServerM)
import Server.Model.AgdaFile (AgdaFile, defNameRange, symbolByName, symbolsByParent)
import Server.Model.Handler (requestHandlerWithAgdaFile)
import Server.Model.Monad (MonadAgdaFile (askAgdaFile))
import Server.Model.Symbol (SymbolInfo (symbolName), SymbolKind (..), lspSymbolKind, symbolKind, symbolShortName, symbolType)

documentSymbolHandler :: LSP.Handlers ServerM
documentSymbolHandler = requestHandlerWithAgdaFile LSP.SMethod_TextDocumentDocumentSymbol $ \_req responder -> do
  file <- askAgdaFile
  let symbols = symbolsByParent file
  let topLevelNames = fromMaybe [] $ Map.lookup Nothing symbols
  let topLevelSymbols = mapMaybe (symbolByName file) topLevelNames
  topLevelDocumentSymbols <- lift $ forMaybeM topLevelSymbols $ symbolToDocumentSymbol file symbols
  responder $ Right $ LSP.InR $ LSP.InL topLevelDocumentSymbols

symbolToDocumentSymbol ::
  AgdaFile ->
  Map (Maybe A.QName) [A.QName] ->
  SymbolInfo ->
  ServerM (Maybe LSP.DocumentSymbol)
symbolToDocumentSymbol file symbolsByParent symbol =
  if symbolIsDocumentSymbol symbol
    then do
      let name = symbolName symbol
      let range = defNameRange file name
      let childrenNames = fromMaybe [] $ Map.lookup (Just name) symbolsByParent
      let childrenSymbols = mapMaybe (symbolByName file) childrenNames
      childrenDocumentSymbols <-
        forMaybeM childrenSymbols $
          symbolToDocumentSymbol file symbolsByParent
      return $
        Just $
          LSP.DocumentSymbol
            (symbolShortName symbol)
            (Text.pack <$> symbolType symbol)
            (lspSymbolKind symbol)
            Nothing
            Nothing
            range
            range
            (Just childrenDocumentSymbols)
    else return Nothing

-- | We only want to report "important" symbols as document symbols. For
-- example, top level module definitions should be document symbols, but local
-- variables should not.
symbolIsDocumentSymbol :: SymbolInfo -> Bool
symbolIsDocumentSymbol symbol = case symbolKind symbol of
  Con -> True
  CoCon -> True
  Field -> True
  PatternSyn -> True
  GeneralizeVar -> True
  Macro -> True
  Data -> True
  Record -> True
  Fun -> True
  Axiom -> True
  Prim -> True
  Module -> True
  Param -> False
  Local -> False
  Unknown -> False
