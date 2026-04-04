module Test.Indexer.Invariants.NoDuplicateDecl (testNoDuplicateDecl) where

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (Pretty, align, pretty, prettyList, prettyShow, text, vcat, (<+>))
import Agda.Utils.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Writer.CPS (Writer, execWriter, tell)
import qualified Data.Map as Map
import Server.Model.AgdaFile (AgdaFile, agdaFileRefs)
import Server.Model.Symbol (Ref, RefKind (..), refKind)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

data DuplicateDecl = DuplicateDecl
  { ddName :: A.QName,
    ddDecls :: [Ref]
  }

instance Pretty DuplicateDecl where
  pretty dupDecl =
    vcat $
      [text "Duplicate declarations for" <+> pretty (ddName dupDecl)]
        <> fmap pretty (ddDecls dupDecl)

  prettyList = \case
    [] -> text "No duplicate declarations"
    [err] -> pretty err
    errs -> align 5 $ (\err -> ("-", vcat [pretty err, text ""])) <$> errs

checkDupDecls :: A.QName -> [Ref] -> Writer [DuplicateDecl] ()
checkDupDecls name refs = do
  let decls = filter (\ref -> refKind ref == Decl) refs
  if length decls > 1
    then tell [DuplicateDecl name decls]
    else return ()

testNoDuplicateDecl :: String -> AgdaFile -> TestTree
testNoDuplicateDecl name agdaFile =
  testCase name $ do
    let symbolRefs = (Map.toList $ agdaFile ^. agdaFileRefs)
    let dupDecls = execWriter $ forM_ symbolRefs $ \(name, refs) ->
          checkDupDecls name refs

    case dupDecls of
      [] -> return ()
      _ -> assertFailure $ prettyShow dupDecls
