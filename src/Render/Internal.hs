{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Render.Internal where

import Control.Monad
import qualified Data.List as List
import qualified Data.Set as Set

import Agda.Syntax.Common (Hiding (..), LensHiding (getHiding), Named (namedThing))
import Agda.Syntax.Internal hiding (telToList)
import Agda.Utils.Function (applyWhen)

import Render.Class
import Render.Common (renderHiding)
import Render.Concrete ()
import Render.RichText

--------------------------------------------------------------------------------

instance Render a => Render (Substitution' a) where
  renderPrec = pr
    where
      pr p input = case input of
        IdS -> "idS"
        EmptyS _ -> "emptyS"
        t :# rho -> mparens (p > 2) $ sep [pr 2 rho <> ",", renderPrec 3 t]
        Strengthen _ _ rho -> mparens (p > 9) $ "strS" <+> pr 10 rho
        Wk n rho -> mparens (p > 9) $ text ("wkS " ++ show n) <+> pr 10 rho
        Lift n rho -> mparens (p > 9) $ text ("liftS " ++ show n) <+> pr 10 rho

-- | Term
instance Render Term where
  renderPrec p val =
    case val of
      Var x els -> text ("@" ++ show x) `pApp` els
      Lam ai b ->
        mparens (p > 0) $
          fsep
            [ "λ" <+> renderHiding ai id (text . absName $ b) <+> "->",
              render (unAbs b)
            ]
      Lit l -> render l
      Def q els -> render q `pApp` els
      Con c _ vs -> render (conName c) `pApp` vs
      Pi a (NoAbs _ b) ->
        mparens (p > 0) $
          fsep
            [ renderPrec 1 (unDom a) <+> "->",
              render b
            ]
      Pi a b ->
        mparens (p > 0) $
          fsep
            [ renderDom (domInfo a) (text (absName b) <+> ":" <+> render (unDom a)) <+> "->",
              render (unAbs b)
            ]
      Sort s -> renderPrec p s
      Level l -> renderPrec p l
      MetaV x els -> render x `pApp` els
      DontCare v -> renderPrec p v
      Dummy s es -> parens (text s) `pApp` es
    where
      pApp d els =
        mparens (not (null els) && p > 9) $
          fsep [d, fsep (fmap (renderPrec 10) els)]

instance (Render t, Render e) => Render (Dom' t e) where
  render dom = pTac <+> renderDom dom (render $ unDom dom)
    where
      pTac
        | Just t <- domTactic dom = "@" <> parens ("tactic" <+> render t)
        | otherwise = mempty

renderDom :: LensHiding a => a -> Inlines -> Inlines
renderDom i =
  case getHiding i of
    NotHidden -> parens
    Hidden -> braces
    Instance {} -> braces . braces

instance Render Clause where
  render Clause {clauseTel = tel, namedClausePats = ps, clauseBody = body, clauseType = ty} =
    sep
      [ render tel <+> "|-",
        fsep
          [ fsep (fmap (renderPrec 10) ps) <+> "=",
            pBody body ty
          ]
      ]
    where
      pBody Nothing _ = "(absurd)"
      pBody (Just b) Nothing = render b
      pBody (Just b) (Just t) = fsep [render b <+> ":", render t]

instance Render a => Render (Tele (Dom a)) where
  render tel = fsep [renderDom a (text x <+> ":" <+> render (unDom a)) | (x, a) <- telToList tel]
    where
      telToList EmptyTel = []
      telToList (ExtendTel a tel') = (absName tel', a) : telToList (unAbs tel')

renderPrecLevelSucs :: Int -> Integer -> (Int -> Inlines) -> Inlines
renderPrecLevelSucs p 0 d = d p
renderPrecLevelSucs p n d = mparens (p > 9) $ "lsuc" <+> renderPrecLevelSucs 10 (n - 1) d

instance Render Level where
  renderPrec p (Max n as) =
    case as of
      [] -> renderN
      [a] | n == 0 -> renderPrec p a
      _ ->
        mparens (p > 9) $
          List.foldr1 (\a b -> "lub" <+> a <+> b) $
            [renderN | n > 0] ++ fmap (renderPrec 10) as
    where
      renderN = renderPrecLevelSucs p n (const "lzero")

instance Render PlusLevel where
  renderPrec p (Plus n a) = renderPrecLevelSucs p n $ \p' -> renderPrec p' a

--instance Render LevelAtom where
-- LevelAtom is just Term
--  renderPrec p a =
--    case a of
--      MetaLevel x els -> renderPrec p (MetaV x els)
--      BlockedLevel _ v -> renderPrec p v
--      NeutralLevel _ v -> renderPrec p v
--      UnreducedLevel v -> renderPrec p v

instance Render Sort where
  renderPrec p = \case
#if MIN_VERSION_Agda(2,6,4)
      Univ u (ClosedLevel n) -> text $ suffix n $ showUniv u
      Univ u l -> mparens (p > 9) $ text (showUniv u) <+> renderPrec 10 l
      Inf u n -> text $ suffix n $ showUniv u ++ "ω"
      LevelUniv -> "LevelUniv"
#else
      Type (ClosedLevel 0) -> "Set"
      Type (ClosedLevel n) -> text $ "Set" ++ show n
      Type l -> mparens (p > 9) $ "Set" <+> renderPrec 10 l
      Prop (ClosedLevel 0) -> "Prop"
      Prop (ClosedLevel n) -> text $ "Prop" ++ show n
      Prop l -> mparens (p > 9) $ "Prop" <+> renderPrec 10 l
      Inf IsFibrant 0 -> "Setω"
      Inf IsStrict 0 -> "SSetω"
      Inf IsFibrant n -> text $ "Setω" ++ show n
      Inf IsStrict n -> text $ "SSetω" ++ show n
      SSet l -> mparens (p > 9) $ "SSet" <+> renderPrec 10 l
#endif
      SizeUniv -> "SizeUniv"
      LockUniv -> "LockUniv"
      PiSort a _s1 s2 ->
        mparens (p > 9) $
          "piSort" <+> renderDom (domInfo a) (text (absName s2) <+> ":" <+> render (unDom a))
            <+> parens
              ( fsep
                  [ text ("λ " ++ absName s2 ++ " ->"),
                    render (unAbs s2)
                  ]
              )
      FunSort a b ->
        mparens (p > 9) $
          "funSort" <+> renderPrec 10 a <+> renderPrec 10 b
      UnivSort s -> mparens (p > 9) $ "univSort" <+> renderPrec 10 s
      MetaS x es -> renderPrec p $ MetaV x es
      DefS d es -> renderPrec p $ Def d es
      DummyS s -> parens $ text s
      IntervalUniv -> "IntervalUniv"
#if MIN_VERSION_Agda(2,6,4)
   where
     suffix n = applyWhen (n /= 0) (++ show n)
#endif

instance Render Type where
  renderPrec p (El _ a) = renderPrec p a

instance Render tm => Render (Elim' tm) where
  renderPrec p (Apply v) = renderPrec p v
  renderPrec _ (Proj _o x) = "." <> render x
  renderPrec p (IApply _ _ r) = renderPrec p r

instance Render DBPatVar where
  renderPrec _ x = text $ patVarNameToString (dbPatVarName x) ++ "@" ++ show (dbPatVarIndex x)

instance Render a => Render (Pattern' a) where
  renderPrec n (VarP _o x) = renderPrec n x
  renderPrec _ (DotP _o t) = "." <> renderPrec 10 t
  renderPrec n (ConP c i nps) =
    mparens (n > 0 && not (null nps)) $
      (lazy <> render (conName c)) <+> fsep (fmap (renderPrec 10) ps)
    where
      ps = fmap (fmap namedThing) nps
      lazy
        | conPLazy i = "~"
        | otherwise = mempty
  renderPrec n (DefP _ q nps) =
    mparens (n > 0 && not (null nps)) $
      render q <+> fsep (fmap (renderPrec 10) ps)
    where
      ps = fmap (fmap namedThing) nps
  -- -- Version with printing record type:
  -- renderPrec _ (ConP c i ps) = (if b then braces else parens) $ prTy $
  --   text (show $ conName c) <+> fsep (fmap (render . namedArg) ps)
  --   where
  --     b = maybe False (== ConOSystem) $ conPRecord i
  --     prTy d = caseMaybe (conPType i) d $ \ t -> d  <+> ":" <+> render t
  renderPrec _ (LitP _ l) = render l
  renderPrec _ (ProjP _o q) = "." <> render q
  renderPrec n (IApplyP _o _ _ x) = renderPrec n x

--------------------------------------------------------------------------------
-- Agda.Syntax.Internal.Blockers

instance Render Blocker where
  render (UnblockOnAll us)      = "all" <> parens (fsep $ punctuate "," $ map render $ Set.toList us)
  render (UnblockOnAny us)      = "any" <> parens (fsep $ punctuate "," $ map render $ Set.toList us)
  render (UnblockOnMeta m)      = render m
  render (UnblockOnProblem pid) = "problem" <+> render pid
  render (UnblockOnDef q)       = "definition" <+> render q
