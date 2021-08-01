{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Render.Concrete where

import qualified Data.Sequence as Seq
import qualified Data.Text     as T
import Data.Maybe (isNothing, maybeToList)
import qualified Data.Strict.Maybe as Strict

import Agda.Syntax.Common
import           Agda.Syntax.Concrete
import           Agda.Syntax.Concrete.Pretty (NamedBinding (..), Tel (..), isLabeled)
import           Agda.Syntax.Position (noRange)
import           Agda.Utils.List1 as List1 (toList, fromList) 
import qualified Agda.Utils.List1 as List1
import qualified Agda.Utils.List2 as List2
import Agda.Utils.Float (toStringWithoutDotZero)
import Agda.Utils.Function (applyWhen)
import Agda.Utils.Functor (dget, (<&>))
import Agda.Utils.Impossible (__IMPOSSIBLE__)

import Render.Class
import Render.Common
import Render.Literal ()
import Render.Name ()
import Render.RichText
import Render.TypeChecking ()

--------------------------------------------------------------------------------

instance Render a => Render (WithHiding a) where
  render w = renderHiding w id $ render $ dget w

-- | OpApp
instance Render (OpApp Expr) where
  render (Ordinary e) = render e
  render (SyntaxBindingLambda r bs e) = render (Lam r bs e)

-- | MaybePlaceholder
instance Render a => Render (MaybePlaceholder a) where
  render Placeholder {} = "_"
  render (NoPlaceholder _ e) = render e

--------------------------------------------------------------------------------

-- | InteractionId
instance Render InteractionId where
  render (InteractionId i) = linkHole i

--------------------------------------------------------------------------------

-- | Expression
instance Render Expr where
  render expr = case expr of
    Ident qname -> render qname
    Lit range lit -> render lit
    -- no hole index, use LinkRange instead
    QuestionMark range Nothing -> linkRange range "?"
    QuestionMark _range (Just n) -> linkHole n
    Underscore range n -> linkRange range $ maybe "_" text n
    -- '_range' is almost always 'NoRange' :(
    App _range _ _ ->
      case appView expr of
        AppView e1 args -> fsep $ render e1 : map render args
    RawApp _ es -> fsep $ map render (List2.toList es)
    OpApp _ q _ es -> fsep $ renderOpApp q es
    WithApp _ e es -> fsep $ render e : map ((text' ["delimiter"] "|" <+>) . render) es
    HiddenArg _ e -> braces' $ render e
    InstanceArg _ e -> dbraces $ render e
    Lam _ bs (AbsurdLam _ h) -> lambda <+> fsep (map render (toList bs)) <+> absurd h
    Lam _ bs e -> sep [lambda <+> fsep (map render (toList bs)) <+> arrow, render e]
    AbsurdLam _ h -> lambda <+> absurd h
    ExtendedLam range _ pes -> lambda <+> bracesAndSemicolons (map render (toList pes))
    Fun _ e1 e2 ->
      sep
        [ renderCohesion e1 (renderQuantity e1 (render e1)) <+> arrow,
          render e2
        ]
    Pi tel e ->
      sep
        [ render (Tel $ smashTel (toList tel)) <+> arrow,
          render e
        ]
    Let _ ds me ->
      sep
        [ "let" <+> vcat (map render (toList ds)),
          maybe mempty (\e -> "in" <+> render e) me
        ]
    Paren _ e -> parens $ render e
    IdiomBrackets _ exprs ->
      case exprs of
        [] -> emptyIdiomBrkt
        [e] -> leftIdiomBrkt <+> render e <+> rightIdiomBrkt
        e : es -> leftIdiomBrkt <+> render e <+> fsep (map (("|" <+>) . render) es) <+> rightIdiomBrkt
    DoBlock _ ss -> "do" <+> vcat (map render (toList ss))
    As _ x e -> render x <> "@" <> render e
    Dot _ e -> "." <> render e
    DoubleDot _ e -> ".." <> render e
    Absurd _ -> "()"
    Rec _ xs -> sep ["record", bracesAndSemicolons (map render xs)]
    RecUpdate _ e xs ->
      sep ["record" <+> render e, bracesAndSemicolons (map render xs)]
    ETel [] -> "()"
    ETel tel -> fsep $ map render tel
    Quote _ -> "quote"
    QuoteTerm _ -> "quoteTerm"
    Unquote _ -> "unquote"
    Tactic _ t -> "tactic" <+> render t
    -- Andreas, 2011-10-03 print irrelevant things as .(e)
    DontCare e -> "." <> parens (render e)
    Equal _ a b -> render a <+> "=" <+> render b
    Ellipsis _ -> "..."
    Generalized e -> render e
    where
      absurd NotHidden = "()"
      absurd Instance {} = "{{}}"
      absurd Hidden = "{}"

-- instance RenderTCM Expr where
--   renderTCM = render

--------------------------------------------------------------------------------

instance (Render a, Render b) => Render (Either a b) where
  render = either render render

instance Render a => Render (FieldAssignment' a) where
  render (FieldAssignment x e) = sep [render x <+> "=", render e]

instance Render ModuleAssignment where
  render (ModuleAssignment m es i) = fsep (render m : map render es) <+> render i

instance Render LamClause where
  render (LamClause lhs rhs _) =
    sep
      [ render lhs,
        render' rhs
      ]
    where
      render' (RHS e) = arrow <+> render e
      render' AbsurdRHS = mempty

instance Render BoundName where
  render BName {boundName = x} = render x

instance Render a => Render (Binder' a) where
  render (Binder mpat n) =
    let d = render n
     in case mpat of
          Nothing -> d
          Just pat -> d <+> "@" <+> parens (render pat)

--------------------------------------------------------------------------------

-- | NamedBinding
instance Render NamedBinding where
  render (NamedBinding withH x) =
    prH $
      if
          | Just l <- isLabeled x -> text l <> " = " <> render xb
          | otherwise -> render xb
    where
      xb = namedArg x
      bn = binderName xb
      prH
        | withH =
          renderRelevance x
            . renderHiding x mparens'
            . renderCohesion x
            . renderQuantity x
            . renderTactic bn
        | otherwise = id
      -- Parentheses are needed when an attribute @... is present
      mparens'
        | noUserQuantity x, Nothing <- bnameTactic bn = id
        | otherwise = parens

renderTactic :: BoundName -> Inlines -> Inlines
renderTactic = renderTactic' . bnameTactic

renderTactic' :: TacticAttribute -> Inlines -> Inlines
renderTactic' Nothing d = d
renderTactic' (Just t) d = "@" <> (parens ("tactic " <> render t) <+> d)

--------------------------------------------------------------------------------

-- | LamBinding
instance Render LamBinding where
  render (DomainFree x) = render (NamedBinding True x)
  render (DomainFull b) = render b

-- | TypedBinding
instance Render TypedBinding where
  render (TLet _ ds) = parens $ "let" <+> vcat (map render (toList ds))
  render (TBind _ xs (Underscore _ Nothing)) =
    fsep (map (render . NamedBinding True) (toList xs))
  render (TBind _ binders e) =
    fsep
      [ renderRelevance y $
          renderHiding y parens $
            renderCohesion y $
              renderQuantity y $
                renderTactic (binderName $ namedArg y) $
                  sep
                    [ fsep (map (render . NamedBinding False) ys),
                      ":" <+> render e
                    ]
        | ys@(y : _) <- groupBinds (toList binders)
      ]
    where
      groupBinds [] = []
      groupBinds (x : xs)
        | Just {} <- isLabeled x = [x] : groupBinds xs
        | otherwise = (x : ys) : groupBinds zs
        where
          (ys, zs) = span (same x) xs
          same a b = getArgInfo a == getArgInfo b && isNothing (isLabeled b)

instance Render Tel where
  render (Tel tel)
    | any isMeta tel = forallQ <+> fsep (map render tel)
    | otherwise = fsep (map render tel)
    where
      isMeta (TBind _ _ (Underscore _ Nothing)) = True
      isMeta _ = False

smashTel :: Telescope -> Telescope
smashTel
  ( TBind r xs e
      : TBind _ ys e'
      : tel
    )
    | show e == show e' = smashTel (TBind r (fromList (toList xs ++ toList ys)) e : tel)
smashTel (b : tel) = b : smashTel tel
smashTel [] = []

instance Render RHS where
  render (RHS e) = "=" <+> render e
  render AbsurdRHS = mempty

instance Render WhereClause where
  render NoWhere = mempty
  render (AnyWhere _range [Module _ x [] ds])
    | isNoName (unqualify x) =
      vcat ["where", vcat $ map render ds]
  render (AnyWhere _range ds) = vcat ["where", vcat $ map render ds]
  render (SomeWhere _range m a ds) =
    vcat
      [ hsep $
          applyWhen
            (a == PrivateAccess UserWritten)
            ("private" :)
            ["module", render m, "where"],
        vcat $ map render ds
      ]

instance Render LHS where
  render (LHS p eqs es) =
    sep
      [ render p,
        if null eqs then mempty else fsep $ map render eqs,
        prefixedThings "with" (map renderWithd es)
      ]
    where
      renderWithd :: WithExpr -> Inlines
      renderWithd (Named nm wh) =
        let e = render wh in
        case nm of
          Nothing -> e
          Just n  -> render n <+> ":" <+> e

instance Render LHSCore where
  render (LHSHead f ps) = sep $ render f : map (parens . render) ps
  render (LHSProj d ps lhscore ps') =
    sep $
      render d :
      map (parens . render) ps
        ++ parens (render lhscore) :
      map (parens . render) ps'
  render (LHSWith h wps ps) =
    if null ps
      then doc
      else sep $ parens doc : map (parens . render) ps
    where
      doc = sep $ render h : map (("|" <+>) . render) wps

instance Render ModuleApplication where
  render (SectionApp _ bs e) = fsep (map render bs) <+> "=" <+> render e
  render (RecordModuleInstance _ rec) = "=" <+> render rec <+> "{{...}}"

instance Render DoStmt where
  render (DoBind _ p e cs) =
    fsep [render p <+> "←", render e, prCs cs]
    where
      prCs [] = mempty
      prCs cs' = fsep ["where", vcat (map render cs')]
  render (DoThen e) = render e
  render (DoLet _ ds) = "let" <+> vcat (map render $ toList ds)

instance Render Declaration where
  render d =
    case d of
      TypeSig i tac x e ->
        sep
          [ renderTactic' tac $ renderRelevance i $ renderCohesion i $ renderQuantity i $ render x <+> ":",
            render e
          ]
      FieldSig inst tac x (Arg i e) ->
        mkInst inst $
          mkOverlap i $
            renderRelevance i $
              renderHiding i id $
                renderCohesion i $
                  renderQuantity i $
                    render $ TypeSig (setRelevance Relevant i) tac x e
        where
          mkInst (InstanceDef _) f = sep ["instance", f]
          mkInst NotInstanceDef f = f

          mkOverlap j f
            | isOverlappable j = "overlap" <+> f
            | otherwise = f
      Field _ fs ->
        sep
          [ "field",
            vcat (map render fs)
          ]
      FunClause lhs rhs wh _ ->
        sep
          [ render lhs,
            render rhs,
            render wh
          ]
      DataSig _ x tel e ->
        fsep
          [ hsep
              [ "data",
                render x,
                fcat (map render tel)
              ],
            hsep
              [ ":",
                render e
              ]
          ]
      Data _ x tel e cs ->
        fsep
          [ hsep
              [ "data",
                render x,
                fcat (map render tel)
              ],
            hsep
              [ ":",
                render e,
                "where"
              ],
            vcat $ map render cs
          ]
      DataDef _ x tel cs ->
        sep
          [ hsep
              [ "data",
                render x,
                fcat (map render tel)
              ],
            "where",
            vcat $ map render cs
          ]
      RecordSig _ x tel e ->
        sep
          [ hsep
              [ "record",
                render x,
                fcat (map render tel)
              ],
            hsep
              [ ":",
                render e
              ]
          ]
      Record _ x dir tel e cs ->
        pRecord x dir tel (Just e) cs
      RecordDef _ x dir tel cs ->
        pRecord x dir tel Nothing cs
      Infix f xs -> render f <+> fsep (punctuate "," $ map render (toList xs))
      Syntax n _ -> "syntax" <+> render n <+> "..."
      PatternSyn _ n as p ->
        "pattern" <+> render n <+> fsep (map render as)
          <+> "="
          <+> render p
      Mutual _ ds -> namedBlock "mutual" ds
      Abstract _ ds -> namedBlock "abstract" ds
      Private _ _ ds -> namedBlock "private" ds
      InstanceB _ ds -> namedBlock "instance" ds
      Macro _ ds -> namedBlock "macro" ds
      Postulate _ ds -> namedBlock "postulate" ds
      Primitive _ ds -> namedBlock "primitive" ds
      Generalize _ ds -> namedBlock "variable" ds
      Module _ x tel ds ->
        fsep
          [ hsep
              [ "module",
                render x,
                fcat (map render tel),
                "where"
              ],
            vcat $ map render ds
          ]
      ModuleMacro _ x (SectionApp _ [] e) DoOpen i
        | isNoName x ->
          fsep
            [ render DoOpen,
              render e,
              render i
            ]
      ModuleMacro _ x (SectionApp _ tel e) open i ->
        fsep
          [ render open <+> "module" <+> render x <+> fcat (map render tel),
            "=" <+> render e <+> render i
          ]
      ModuleMacro _ x (RecordModuleInstance _ rec) open _ ->
        fsep
          [ render open <+> "module" <+> render x,
            "=" <+> render rec <+> "{{...}}"
          ]
      Open _ x i -> hsep ["open", render x, render i]
      Import _ x rn open i ->
        hsep [render open, "import", render x, as rn, render i]
        where
          as Nothing = mempty
          as (Just y) = "as" <+> render (asName y)
      UnquoteDecl _ xs t ->
        fsep ["unquoteDecl" <+> fsep (map render xs) <+> "=", render t]
      UnquoteDef _ xs t ->
        fsep ["unquoteDef" <+> fsep (map render xs) <+> "=", render t]
      Pragma pr -> sep ["{-#" <+> render pr, "#-}"]
    where
      namedBlock s ds =
        fsep
          [ text s,
            vcat $ map render ds
          ]

pRecord ::
  Name ->
  RecordDirectives ->
  [LamBinding] ->
  Maybe Expr ->
  [Declaration] ->
  Inlines
pRecord x (RecordDirectives ind eta pat con) tel me cs =
  sep
    [ hsep
        [ "record",
          render x,
          fcat (map render tel)
        ],
      pType me,
      vcat $
        pInd
          ++ pEta
          ++ pCon
          ++ map render cs
    ]
  where
    pType (Just e) =
      hsep
        [ ":",
          render e,
          "where"
        ]
    pType Nothing =
      "where"
    pInd = maybeToList $ text . show . rangedThing <$> ind
    pEta =
      maybeToList $
        eta <&> \case
          YesEta -> "eta-equality"
          NoEta _ -> "no-eta-equality"
    pCon = maybeToList $ (("constructor" <+>) . render) . fst <$> con

instance Render OpenShortHand where
  render DoOpen = "open"
  render DontOpen = mempty

instance Render Pragma where
  render (OptionsPragma _ opts) = fsep $ map text $ "OPTIONS" : opts
  render (BuiltinPragma _ b x) = hsep ["BUILTIN", text (rangedThing b), render x]
  render (RewritePragma _ _ xs) =
    hsep ["REWRITE", hsep $ map render xs]
  render (CompilePragma _ b x e) =
    hsep ["COMPILE", text (rangedThing b), render x, text e]
  render (ForeignPragma _ b s) =
    vcat $ text ("FOREIGN " ++ rangedThing b) : map text (lines s)
  render (StaticPragma _ i) =
    hsep ["STATIC", render i]
  render (InjectivePragma _ i) =
    hsep ["INJECTIVE", render i]
  render (InlinePragma _ True i) =
    hsep ["INLINE", render i]
  render (InlinePragma _ False i) =
    hsep ["NOINLINE", render i]
  render (ImpossiblePragma _ strs) =
    hsep $ "IMPOSSIBLE" : map text strs
  render (EtaPragma _ x) =
    hsep ["ETA", render x]
  render (TerminationCheckPragma _ tc) =
    case tc of
      TerminationCheck -> __IMPOSSIBLE__
      NoTerminationCheck -> "NO_TERMINATION_CHECK"
      NonTerminating -> "NON_TERMINATING"
      Terminating -> "TERMINATING"
      TerminationMeasure _ x -> hsep ["MEASURE", render x]
  render (NoCoverageCheckPragma _) = "NON_COVERING"
  render (WarningOnUsage _ nm str) = hsep ["WARNING_ON_USAGE", render nm, text $ T.unpack str]
  render (WarningOnImport _ str) = hsep ["WARNING_ON_IMPORT", text $ T.unpack str]
  render (CatchallPragma _) = "CATCHALL"
  render (DisplayPragma _ lhs rhs) = "DISPLAY" <+> fsep [render lhs <+> "=", render rhs]
  render (NoPositivityCheckPragma _) = "NO_POSITIVITY_CHECK"
  render (PolarityPragma _ q occs) =
    hsep ("POLARITY" : render q : map render occs)
  render (NoUniverseCheckPragma _) = "NO_UNIVERSE_CHECK"

instance Render Fixity where
  render (Fixity _ Unrelated _) = __IMPOSSIBLE__
  render (Fixity _ (Related d) ass) = s <+> text (toStringWithoutDotZero d)
    where
      s = case ass of
        LeftAssoc -> "infixl"
        RightAssoc -> "infixr"
        NonAssoc -> "infix"

instance Render GenPart where
  render (IdPart x) = text $ rangedThing x
  render BindHole {} = "_"
  render NormalHole {} = "_"
  render WildHole {} = "_"

instance Render Fixity' where
  render (Fixity' fix nota _)
    | nota == noNotation = render fix
    | otherwise = "syntax" <+> render nota

-- | Arg
instance Render a => Render (Arg a) where
  renderPrec p (Arg ai e) = renderHiding ai localParens $ renderPrec p' e
    where
      p'
        | visible ai = p
        | otherwise = 0
      localParens
        | getOrigin ai == Substitution = parens
        | otherwise = id

-- | Named NamedName (Named_)
instance Render e => Render (Named NamedName e) where
  renderPrec p (Named nm e)
    | Just s <- bareNameOf nm = mparens (p > 0) $ sep [text s <> " =", render e]
    | otherwise = renderPrec p e

instance Render Pattern where
  render = \case
    IdentP x -> render x
    AppP p1 p2 -> fsep [render p1, render p2]
    RawAppP _ ps -> fsep $ map render (List2.toList ps)
    OpAppP _ q _ ps -> fsep $ renderOpApp q (fmap (fmap (fmap (NoPlaceholder Strict.Nothing))) ps)
    HiddenP _ p -> braces' $ render p
    InstanceP _ p -> dbraces $ render p
    ParenP _ p -> parens $ render p
    WildP _ -> "_"
    AsP _ x p -> render x <> "@" <> render p
    DotP _ p -> "." <> render p
    AbsurdP _ -> "()"
    LitP _ l -> render l
    QuoteP _ -> "quote"
    RecP _ fs -> sep ["record", bracesAndSemicolons (map render fs)]
    EqualP _ es -> sep $ [parens (sep [render e1, "=", render e2]) | (e1, e2) <- es]
    EllipsisP _ mp -> "..."
    WithP _ p -> "|" <+> render p

bracesAndSemicolons :: [Inlines] -> Inlines
bracesAndSemicolons [] = "{}"
bracesAndSemicolons (d : ds) = sep (["{" <+> d] ++ map (";" <+>) ds ++ ["}"])

renderOpApp ::
  forall a.
  Render a =>
  QName ->
  [NamedArg (MaybePlaceholder a)] ->
  [Inlines]
renderOpApp q args = merge [] $ prOp moduleNames concreteNames args
  where
    -- ms: the module part of the name.
    moduleNames = List1.init (qnameParts q)
    -- xs: the concrete name (alternation of @Id@ and @Hole@)
    concreteNames = case unqualify q of
      Name _ _ xs -> List1.toList xs
      NoName {} -> __IMPOSSIBLE__

    prOp :: Render a => [Name] -> [NamePart] -> [NamedArg (MaybePlaceholder a)] -> [(Inlines, Maybe PositionInName)]
    prOp ms (Hole : xs) (e : es) =
      case namedArg e of
        Placeholder p -> (qual ms $ render e, Just p) : prOp [] xs es
        NoPlaceholder {} -> (render e, Nothing) : prOp ms xs es
    -- Module qualifier needs to go on section holes (#3072)
    prOp _ (Hole : _) [] = __IMPOSSIBLE__
    prOp ms (Id x : xs) es =
      ( qual ms $ render $ simpleName x,
        Nothing
      ) :
      prOp [] xs es
    -- Qualify the name part with the module.
    -- We then clear @ms@ such that the following name parts will not be qualified.

    prOp _ [] es = map (\e -> (render e, Nothing)) es

    qual ms' doc = hcat $ punctuate "." $ map render ms' ++ [doc]

    -- Section underscores should be printed without surrounding
    -- whitespace. This function takes care of that.
    merge :: [Inlines] -> [(Inlines, Maybe PositionInName)] -> [Inlines]
    merge before [] = reverse before
    merge before ((d, Nothing) : after) = merge (d : before) after
    merge before ((d, Just Beginning) : after) = mergeRight before d after
    merge before ((d, Just End) : after) = case mergeLeft d before of
      (d', bs) -> merge (d' : bs) after
    merge before ((d, Just Middle) : after) = case mergeLeft d before of
      (d', bs) -> mergeRight bs d' after

    mergeRight before d after =
      reverse before
        ++ case merge [] after of
          [] -> [d]
          a : as -> (d <> a) : as

    mergeLeft d before = case before of
      [] -> (d, [])
      b : bs -> (b <> d, bs)

instance (Render a, Render b) => Render (ImportDirective' a b) where
  render i =
    sep
      [ public (publicOpen i),
        render $ using i,
        renderHiding' $ hiding i,
        rename $ impRenaming i
      ]
    where
      public Just {} = "public"
      public Nothing = mempty

      renderHiding' [] = mempty
      renderHiding' xs = "hiding" <+> parens (fsep $ punctuate ";" $ map render xs)

      rename [] = mempty
      rename xs =
        hsep
          [ "renaming",
            parens $ fsep $ punctuate ";" $ map render xs
          ]

instance (Render a, Render b) => Render (Using' a b) where
  render UseEverything = mempty
  render (Using xs) =
    "using" <+> parens (fsep $ punctuate ";" $ map render xs)

instance (Render a, Render b) => Render (Renaming' a b) where
  render (Renaming from to mfx _r) =
    hsep
      [ render from,
        "to",
        maybe mempty render mfx,
        case to of
          ImportedName a -> render a
          ImportedModule b -> render b -- don't print "module" here
      ]

instance (Render a, Render b) => Render (ImportedName' a b) where
  render (ImportedName a) = render a
  render (ImportedModule b) = "module" <+> render b
