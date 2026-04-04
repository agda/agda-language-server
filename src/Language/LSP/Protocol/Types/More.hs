module Language.LSP.Protocol.Types.More () where

import Agda.Syntax.Common.Pretty
import Agda.Utils.Lens ((^.))
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Types.Uri.More as LSP

instance Pretty LSP.Uri where
  pretty = text . Text.unpack . LSP.getUri

instance Pretty LSP.NormalizedUri where
  pretty = text . Text.unpack . LSP.getNormalizedUri

instance Pretty LSP.Position where
  pretty pos = pshow (pos ^. LSP.line + 1) <> text ":" <> pshow (pos ^. LSP.character + 1)

instance Pretty LSP.Range where
  pretty range =
    if range ^. LSP.start . LSP.line == range ^. LSP.end . LSP.line
      then
        pshow (range ^. LSP.start . LSP.line + 1)
          <> text ":"
          <> pshow (range ^. LSP.start . LSP.character + 1)
          <> text "-"
          <> pshow (range ^. LSP.end . LSP.character + 1)
      else pretty (range ^. LSP.start) <> text "-" <> pretty (range ^. LSP.end)
