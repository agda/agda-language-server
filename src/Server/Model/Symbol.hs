module Server.Model.Symbol
  ( SymbolKind (..),
    SymbolInfo (..),
    symbolShortName,
    lspSymbolKind,
    RefKind (..),
    Ref (..),
    refIsDef,
  )
where

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common.Pretty (Doc, Pretty, comma, parensNonEmpty, pretty, prettyShow, pshow, text, (<+>))
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types.More ()

data SymbolKind
  = Con
  | CoCon
  | Field
  | PatternSyn
  | GeneralizeVar
  | Macro
  | Data
  | Record
  | Fun
  | Axiom
  | Prim
  | Module
  | Param
  | Local
  | Unknown
  deriving (Show, Eq)

instance Pretty SymbolKind where
  pretty = pshow

instance Semigroup SymbolKind where
  Unknown <> k = k
  k <> _k = k

toLspSymbolKind :: SymbolKind -> LSP.SymbolKind
toLspSymbolKind = \case
  Con -> LSP.SymbolKind_Constructor
  CoCon -> LSP.SymbolKind_Constructor
  Field -> LSP.SymbolKind_Field
  PatternSyn -> LSP.SymbolKind_Function
  GeneralizeVar -> LSP.SymbolKind_Variable
  Macro -> LSP.SymbolKind_Function
  Data -> LSP.SymbolKind_Enum
  Record -> LSP.SymbolKind_Struct
  Fun -> LSP.SymbolKind_Function
  Axiom -> LSP.SymbolKind_Constant
  Prim -> LSP.SymbolKind_Constant
  Module -> LSP.SymbolKind_Module
  Param -> LSP.SymbolKind_Variable
  Local -> LSP.SymbolKind_Variable
  Unknown -> LSP.SymbolKind_Variable

data SymbolInfo = SymbolInfo
  { symbolName :: !A.QName,
    symbolKind :: !SymbolKind,
    symbolType :: !(Maybe String),
    symbolParent :: !(Maybe A.QName)
  }
  deriving (Eq)

instance Pretty SymbolInfo where
  pretty symbolInfo =
    pretty (symbolKind symbolInfo)
      <+> parensNonEmpty (pretty $ symbolType symbolInfo)

instance Semigroup SymbolInfo where
  new <> old =
    SymbolInfo
      (symbolName new)
      (symbolKind old <> symbolKind new)
      (symbolType old <|> symbolType new)
      (symbolParent old <|> symbolParent new)

symbolShortName :: SymbolInfo -> Text
symbolShortName = Text.pack . prettyShow . A.qnameName . symbolName

lspSymbolKind :: SymbolInfo -> LSP.SymbolKind
lspSymbolKind = toLspSymbolKind . symbolKind

data RefKind
  = -- | The symbol is being declared. There should be at most one declaration
    -- for any given symbol (in correct Agda code). Roughly speaking, this is
    -- the "single most important defining reference"
    --
    -- For example, a function's name in its signature
    Decl
  | -- | The symbol is being defined, but is not being declared in the sense
    -- that @Decl@ would apply. Typically, this means the definition may be
    -- split into several parts, and this is one of the "less important" parts
    --
    -- For example, a function's name in the LHS of a clause in its definition
    Def
  | -- | The symbol is (expected to be) already defined and is being used
    --
    -- For example, a function's name in function application
    Usage
  | -- | The symbol is being imported here
    Import
  deriving (Show, Eq)

instance Pretty RefKind where
  pretty = pshow

data Ref = Ref
  { refKind :: !RefKind,
    refRange :: !LSP.Range,
    refIsAmbiguous :: !Bool
  }
  deriving (Eq)

prettyAmbiguity :: Ref -> Doc
prettyAmbiguity ref =
  if refIsAmbiguous ref
    then text "ambiguous"
    else text "unambiguous"

instance Pretty Ref where
  pretty ref =
    ((prettyAmbiguity ref <+> pretty (refKind ref)) <> comma)
      <+> pretty (refRange ref)

refIsDef :: Ref -> Bool
refIsDef ref = refKind ref == Def
