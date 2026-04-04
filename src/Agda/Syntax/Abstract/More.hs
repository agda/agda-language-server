{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Agda.Syntax.Abstract.More () where

import Agda.Syntax.Abstract
import Agda.Syntax.Common
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Info
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map

instance Pretty Declaration where
  pretty decl = prettyAssign $ case decl of
    Axiom kindOfName _defInfo _argInfo _polarities name type' ->
      ( text "Axiom",
        prettyMap
          [ (text "kindOfName", pshow kindOfName),
            (text "name", pretty name),
            (text "type", pretty type')
          ]
      )
    Generalize _namesInType _defInfo _argInfo name type' ->
      ( text "Generalize",
        prettyMap
          [ (text "name", pretty name),
            (text "type", pretty type')
          ]
      )
    Field _defInfo name type' ->
      ( text "Field",
        prettyMap
          [ (text "name", pretty name),
            (text "type", pretty type')
          ]
      )
    Primitive _defInfo name type' ->
      ( text "Primitive",
        prettyMap
          [ (text "name", pretty name),
            (text "type", pretty type')
          ]
      )
    Mutual _mutualInfo decls -> (text "Mutual", pretty decls)
    Section _range _erased moduleName genTel decls ->
      ( text "Section",
        prettyMap
          [ (text "moduleName", pretty moduleName),
            (text "genTel", pretty genTel),
            (text "decls", pretty decls)
          ]
      )
    Apply _moduleInfo _erased moduleName _moduleApp _scopeCopyInfo _importDirective ->
      (text "Apply", pretty moduleName)
    Import _moduleInfo moduleName _importDirective -> (text "Import", pretty moduleName)
    Pragma _range _pragma -> (text "Pragma", mempty)
    Open _moduleInfo moduleName _importDirective ->
      (text "Open", pretty moduleName)
    FunDef _defInfo name clauses ->
      ( text "FunDef",
        prettyMap
          [ (text "name", pretty name),
            (text "clauses", pretty clauses)
          ]
      )
    DataSig _defInfo _erased name _genTel type' ->
      ( text "DataSig",
        prettyMap
          [ (text "name", pretty name),
            (text "type", pretty type')
          ]
      )
    DataDef _defIngo name _univCheck dataDefParams _ctors ->
      ( text "DataDef",
        prettyMap
          [ (text "name", pretty name),
            (text "dataDefParams", pretty dataDefParams)
          ]
      )
    RecSig _defInfo _erased name genTel type' ->
      ( text "RecSig",
        prettyMap
          [ (text "name", pretty name),
            (text "genTel", pretty genTel),
            (text "type", pretty type')
          ]
      )
    RecDef _defInfo name _univCheck _recDirs dataDefParams _type' decls ->
      ( text "RecDef",
        prettyMap
          [ (text "name", pretty name),
            (text "dataDefParams", pretty dataDefParams),
            (text "decls", pretty decls)
          ]
      )
    PatternSynDef name _args _body -> (text "PatternSynDef", pretty name)
    -- ...
    ScopedDecl scopeInfo decls -> (text "ScopedDecl", pretty decls)
    -- ...
    _ -> ("Decl", mempty)

instance Pretty Expr where
  pretty expr = prettyAssign $ case expr of
    Var name -> (text "Var", pretty name)
    Def' name _suffix -> (text "Def'", pretty name)
    Proj _origin name -> (text "Proj", pretty name)
    Con name -> (text "Con", pretty name)
    PatternSyn name -> (text "PatternSyn", pretty name)
    Macro name -> (text "Macro", pretty name)
    -- ...
    App _appInfo f arg ->
      ( text "App",
        prettyMap
          [ (text "f", pretty f),
            (text "arg", pretty arg)
          ]
      )
    -- ...
    Pi _exprInfo dom codom ->
      ( text "Pi",
        prettyMap
          [ (text "dom", pretty dom),
            (text "codom", pretty codom)
          ]
      )
    Generalized dom codom ->
      ( text "Generalized",
        prettyMap
          [ (text "dom", pretty $ toList dom),
            (text "codom", pretty codom)
          ]
      )
    Fun _exprInfo dom codom ->
      ( text "Fun",
        prettyMap
          [ (text "dom", pretty dom),
            (text "codom", pretty codom)
          ]
      )
    Let _exprInfo bindings body ->
      ( text "Let",
        prettyMap
          [ (text "bindings", pretty bindings),
            (text "body", pretty body)
          ]
      )
    -- ...
    ScopedExpr _scopeInfo expr -> (text "ScopedExpr", pretty expr)
    -- ...
    _ -> ("Expr", mempty)

instance (Pretty a) => Pretty (Pattern' a) where
  pretty _pat = text "pat"

debugNamedBinder :: NamedArg Binder -> Doc
debugNamedBinder (Arg argInfo binder) = pshow (argInfoOrigin argInfo) <+> pretty binder

instance Pretty TypedBinding where
  pretty = \case
    TBind _range _typedBindingInfo binders type' ->
      parens (prettyList (debugNamedBinder <$> toList binders) <+> colon <+> pretty type')
    TLet _range _letBindings -> text "TLet"

instance Pretty LetBinding where
  pretty = \case
    LetBind _letInfo _argInfo name type' expr ->
      text "let" <+> pretty (unBind name) <+> text "=" <+> pretty expr
    LetPatBind _letInfo pat expr ->
      text "letPat" <+> pretty pat <+> text "=" <+> pretty expr
    _ -> text "LetBinding"

instance Pretty Binder where
  pretty binder = pretty $ unBind $ binderName binder

instance Pretty LamBinding where
  pretty = \case
    DomainFree _tacticAttr namedArgBinder -> debugNamedBinder namedArgBinder
    DomainFull binding -> pretty binding

instance (Pretty t) => Pretty (DefInfo' t) where
  pretty defInfo =
    align
      20
      [("DefInfo", prettyMap [(text "defTactic", defTactic defInfo)])]

instance Pretty GeneralizeTelescope where
  pretty genTel =
    align
      20
      [ ( "GeneralizeTelescope",
          prettyMap
            [ (text "generalizeTelVars", prettyMap $ Map.toList $ generalizeTelVars genTel),
              (text "generalizeTel", pretty $ generalizeTel genTel)
            ]
        )
      ]

instance (Pretty lhs) => Pretty (Clause' lhs) where
  pretty (Clause lhs _strippedPats rhs whereDecls _catchall) =
    pretty lhs <+> text "=" <+> pretty rhs

instance Pretty LHS where
  pretty _lhs = text "lhs"

instance Pretty RHS where
  pretty = \case
    RHS expr _concrete -> pretty expr
    _ -> text "rhs"

instance Pretty DataDefParams where
  pretty (DataDefParams _generalized params) = pretty params
