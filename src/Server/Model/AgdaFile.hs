module Server.Model.AgdaFile
  ( AgdaFile,
    emptyAgdaFile,
    agdaFileSymbols,
    agdaFileRefs,
    insertSymbolInfo,
    insertRef,
    mergeSymbols,
    symbolByName,
    symbolsByParent,
    defNameRange,
  )
where

import Agda.Position (toLspRange)
import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (Pretty, pretty, prettyAssign, prettyMap, prettyShow, text, vcat)
import Agda.Syntax.Position (getRange)
import Agda.Utils.Lens (Lens', over, (<&>), (^.))
import Data.Foldable (find)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (Endo, appEndo))
import qualified Language.LSP.Protocol.Types as LSP
import Server.Model.Symbol (Ref, SymbolInfo, refIsDef, refRange, symbolParent)

data AgdaFile = AgdaFile
  { _agdaFileSymbols :: !(Map A.QName SymbolInfo),
    _agdaFileRefs :: !(Map A.QName [Ref])
  }
  deriving (Eq)

instance Pretty AgdaFile where
  pretty agdaFile =
    vcat
      [ prettyAssign (text "symbols", prettyMap $ Map.toList $ agdaFile ^. agdaFileSymbols),
        prettyAssign (text "refs", prettyMap $ Map.toList $ agdaFile ^. agdaFileRefs)
      ]

instance Show AgdaFile where
  show = prettyShow

emptyAgdaFile :: AgdaFile
emptyAgdaFile = AgdaFile Map.empty Map.empty

agdaFileSymbols :: Lens' AgdaFile (Map A.QName SymbolInfo)
agdaFileSymbols f a = f (_agdaFileSymbols a) <&> \x -> a {_agdaFileSymbols = x}

agdaFileRefs :: Lens' AgdaFile (Map A.QName [Ref])
agdaFileRefs f a = f (_agdaFileRefs a) <&> \x -> a {_agdaFileRefs = x}

insertSymbolInfo ::
  A.QName ->
  SymbolInfo ->
  AgdaFile ->
  AgdaFile
insertSymbolInfo name symbolInfo =
  over agdaFileSymbols $ Map.insertWith (<>) name symbolInfo

insertRef :: A.AmbiguousQName -> Ref -> AgdaFile -> AgdaFile
insertRef ambiguousName ref =
  over agdaFileRefs $
    appEndo $
      foldMap (\name -> Endo $ Map.insertWith (<>) name [ref]) (A.unAmbQ ambiguousName)

mergeSymbols :: A.QName -> A.QName -> AgdaFile -> AgdaFile
mergeSymbols old new file =
  file
    & over
      agdaFileSymbols
      ( \symbols ->
          let (oldSymbolInfo, symbols') =
                Map.updateLookupWithKey (\_ _ -> Nothing) old symbols
           in Map.alter (<> oldSymbolInfo) new symbols'
      )
    & over
      agdaFileRefs
      ( \refs ->
          let (oldRefs, refs') =
                Map.updateLookupWithKey (\_ _ -> Nothing) old refs
           in Map.alter (<> oldRefs) new refs'
      )

symbolByName :: AgdaFile -> A.QName -> Maybe SymbolInfo
symbolByName agdaFile symbolName = Map.lookup symbolName $ agdaFile ^. agdaFileSymbols

symbolsByParent :: AgdaFile -> Map (Maybe A.QName) [A.QName]
symbolsByParent agdaFile =
  let symbols = Map.toList $ agdaFile ^. agdaFileSymbols
   in Map.fromListWith (++) $ (\(symbolName, symbol) -> (symbolParent symbol, [symbolName])) <$> symbols

refsByName :: AgdaFile -> A.QName -> [Ref]
refsByName agdaFile name = Map.findWithDefault [] name $ agdaFile ^. agdaFileRefs

defNameRange :: AgdaFile -> A.QName -> LSP.Range
defNameRange agdaFile name =
  let defs = find refIsDef $ refsByName agdaFile name
   in maybe (toLspRange $ getRange name) refRange defs
