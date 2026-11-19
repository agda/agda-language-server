{-# LANGUAGE CPP #-}

-- | Rendering tests (#73).
--
-- Each test checks one property: for a concrete syntax tree @t@, the
-- plain text of ALS's @render t@ is the same as Agda's own @prettyShow t@,
-- up to whitespace. The layout is the client's job; the text must match.
--
-- The coverage is partial: it has only the renderer cases that changed
-- from Agda 2.8.0 to 2.9.0, and assumes that the rest renders correctly.
-- Covering the rest of the renderer with the same property is planned.
module Test.Render (tests) where

import Test.Tasty

#if MIN_VERSION_Agda(2,9,0)
import Agda.Syntax.Abstract.Name (mkName_)
import Agda.Syntax.Common (NameId (..), defaultNamedArg, noPlaceholder)
import Agda.Syntax.Common.Aspect (Aspect, Aspects (..))
import qualified Agda.Syntax.Common.Aspect as Asp
import Agda.Syntax.Common.Pretty (Pretty, prettyShow)
import Agda.Syntax.Concrete
import Agda.Syntax.Concrete.Pretty ()
import Agda.Syntax.Parser (exprParser, moduleParser, parse, parseFile, runPMIO)
import Agda.Syntax.Position (RangeFile (RangeFile), noRange)
import Agda.Syntax.TopLevelModuleName.Boot (noModuleNameHash)
import Agda.Utils.FileName (mkAbsolute)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Agda.Utils.Null (empty)
import qualified Agda.Utils.Set1 as Set1
import Render (Render, render, toPlainText)
import Test.Tasty.HUnit
#endif

tests :: TestTree
#if MIN_VERSION_Agda(2,9,0)
tests =
  testGroup
    "Render"
    [ testCase "open import with arguments" $ do
        d <- importDecl
        agrees d "open import M a",
      testCase "open import with an as name" $ do
        Import open kw x _ dir <- importDecl
        n <- nameOf "N"
        agrees (Import open kw x (Left (AsName n noRange)) dir) "open import M as N",
      testCase "open with arguments" $ do
        ds <- parseDecls "open M a"
        case ds of
          [Private _ _ [d@ModuleMacro {}]] -> agrees d "open M a"
          _ -> assertFailure ("not one inserted module macro: " ++ show (length ds)),
      testCase "open module" $ do
        d <- oneDecl "open module X = M"
        agrees d "open module X = M",
      testCase "dot pattern" $ do
        ds <- parseDecls "f : A → A\nf .x = x"
        case ds of
          [_, d] -> agrees d "f .x = x"
          _ -> assertFailure ("not two declarations: " ++ show (length ds)),
      testCase "ETA_EQUALITY pragma" $ do
        d <- oneDecl "{-# ETA_EQUALITY #-}"
        agrees d "{-# ETA_EQUALITY #-}",
      testCase "sharp cohesion" $ do
        e <- parseExpr "(@♯ x : A) → A"
        agrees e "(@♯ x : A) → A",
      testCase "highlighted expression" $ do
        e <- parseExpr "f x"
        agrees (Highlighted empty e) "f x",
      testCase "known operator application with an aspect" $ do
        e <- knownOpApp (Just (Asp.Name (Just Asp.Function) True))
        agrees e "x + y",
      testCase "known operator application without an aspect" $ do
        e <- knownOpApp Nothing
        agrees e "x + y"
    ]

-- | Asserts Agda's text first, so a failure shows whether the snippet has
-- the feature, then that ALS's text is the same.
agrees :: (Render a, Pretty a) => a -> String -> Assertion
agrees t expected = do
  normalize (prettyShow t) @?= expected
  normalize (toPlainText (render t)) @?= expected
  where
    normalize = unwords . words

-- | The declarations inside @module M where@, followed by the lines of
-- the snippet
parseDecls :: String -> IO [Declaration]
parseDecls src = do
  let file = RangeFile (mkAbsolute "/M.agda") Nothing
  (result, _warnings) <- runPMIO $ parseFile False moduleParser file ("module M where\n\n" ++ src ++ "\n")
  case result of
    Left err -> assertFailure ("parse error: " ++ prettyShow err)
    Right (((Mod _ _ [Module _ _ _ _ ds], _attrs), _fileType)) -> return ds
    Right _ -> assertFailure "not one top-level module"

oneDecl :: String -> IO Declaration
oneDecl src = do
  ds <- parseDecls src
  case ds of
    [d] -> return d
    _ -> assertFailure ("not one declaration: " ++ show (length ds))

parseExpr :: String -> IO Expr
parseExpr src = do
  (result, _warnings) <- runPMIO $ parse exprParser src
  case result of
    Left err -> assertFailure ("parse error: " ++ prettyShow err)
    Right (e, _attrs) -> return e

importDecl :: IO Declaration
importDecl = do
  d <- oneDecl "open import M a"
  case d of
    Import _ _ _ (Right _) _ -> return d
    _ -> assertFailure "not an import with arguments"

nameOf :: String -> IO Name
nameOf s = do
  e <- parseExpr s
  case e of
    Ident (QName n) -> return n
    _ -> assertFailure ("not an unqualified name: " ++ s)

-- | @x + y@ as the type checker gives it back: 'KnownOpApp' is built only
-- when abstract syntax is turned back into concrete syntax
knownOpApp :: Maybe Aspect -> IO Expr
knownOpApp asp = do
  op <- parseExpr "_+_"
  x <- parseExpr "x"
  y <- parseExpr "y"
  case op of
    Ident q@(QName n) ->
      return $
        KnownOpApp
          empty {aspect = asp}
          noRange
          q
          (Set1.singleton (mkName_ (NameId 0 noModuleNameHash) (prettyShow n)))
          (arg x :| [arg y])
    _ -> assertFailure "not an unqualified operator"
  where
    arg = defaultNamedArg . noPlaceholder . Ordinary
#else
tests = testGroup "Render" []
#endif
