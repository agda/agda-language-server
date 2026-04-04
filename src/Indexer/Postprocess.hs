module Indexer.Postprocess (postprocess) where

import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Common as C
import Agda.Utils.Lens (over)
import Control.Arrow ((>>>))
import Data.Foldable (Foldable (toList), find)
import qualified Data.Map as Map
import Indexer.Monad (DataRecordParams (DataRecordParams), IndexerM, getDataRecordParams, modifyAgdaFile')
import Server.Model.AgdaFile (AgdaFile, agdaFileRefs, mergeSymbols)
import Server.Model.Symbol (Ref, RefKind (..), refKind, refRange)

postprocess :: IndexerM ()
postprocess = do
  dataRecordParams <- Map.elems <$> getDataRecordParams
  modifyAgdaFile' $
    dedupDataRecordParams dataRecordParams
      >>> dedupSimultaneousDeclDef

-- | We sometimes emit a `Decl` and `Def` for the same source range. For
-- example, we must emit a distinct `Decl` and `Def` when a `data` is declared
-- and defined separately, but not when it is declared and defined all at once.
-- It is harder distinguish these in abstract syntax than to idenfify and
-- correct them at the end.
dedupSimultaneousDeclDef :: AgdaFile -> AgdaFile
dedupSimultaneousDeclDef = over agdaFileRefs $ Map.map worker
  where
    worker :: [Ref] -> [Ref]
    worker refs =
      case find (\ref -> refKind ref == Decl) refs of
        Nothing -> refs
        Just decl ->
          let pred ref = not (refKind ref == Def && refRange ref == refRange decl)
           in filter pred refs

dedupDataRecordParams :: [DataRecordParams] -> AgdaFile -> AgdaFile
dedupDataRecordParams dataRecordParams file = foldl' (flip worker) file dataRecordParams
  where
    namedArgBinderName :: C.NamedArg A.Binder -> A.QName
    namedArgBinderName namedArgBinder =
      A.qualify_ $ A.unBind $ A.binderName $ C.namedThing $ C.unArg namedArgBinder
    typedBindingNames :: A.TypedBinding -> [A.QName]
    typedBindingNames = \case
      A.TBind _range _typedBindingInfo nabs _type' ->
        namedArgBinderName <$> toList nabs
      A.TLet _range _letBindings -> []
    lamBindingNames :: A.LamBinding -> [A.QName]
    lamBindingNames = \case
      A.DomainFree _tacticAttr nab -> [namedArgBinderName nab]
      A.DomainFull typedBindings -> typedBindingNames typedBindings
    worker :: DataRecordParams -> AgdaFile -> AgdaFile
    worker (DataRecordParams (Just genTel) (Just params)) file =
      let paramsNames = concatMap lamBindingNames $ A.dataDefParams params
          genTelNames = concatMap typedBindingNames $ A.generalizeTel genTel
          names = zip paramsNames genTelNames
       in foldl' (\file (old, new) -> mergeSymbols old new file) file names
    worker _missingParams file = file
