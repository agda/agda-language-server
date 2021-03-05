{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleInstances #-}
module Render.Internal where

import Agda.Syntax.Internal hiding (renderDom)
import Render.Class
import Render.RichText
import Render.Common (renderHiding)
import Render.Concrete ()
import Render.Literal
import Agda.Syntax.Common (LensHiding (getHiding), Hiding (..), Named (namedThing))
import qualified Data.List as List

--------------------------------------------------------------------------------

instance Render a => Render (Substitution' a) where
  renderPrec = pr
    where
    pr p rho = case rho of
      IdS              -> "idS"
      EmptyS err       -> "emptyS"
      t :# rho         -> mparens (p > 2) $ sep [ pr 2 rho <> ",", renderPrec 3 t ]
      Strengthen _ rho -> mparens (p > 9) $ "strS" <+> pr 10 rho
      Wk n rho         -> mparens (p > 9) $ text ("wkS " ++ show n) <+> pr 10 rho
      Lift n rho       -> mparens (p > 9) $ text ("liftS " ++ show n) <+> pr 10 rho

-- | Term 
instance Render Term where
  renderPrec p v =
    case v of
      Var x els -> text ("@" ++ show x) `pApp` els
      Lam ai b   ->
        mparens (p > 0) $
        sep [ "λ" <+> renderHiding ai id (text . absName $ b) <+> "->"
            , indent $ render (unAbs b) ]
      Lit l                -> render l
      Def q els            -> render q `pApp` els
      Con c ci vs          -> render (conName c) `pApp` vs
      Pi a (NoAbs _ b)     -> mparens (p > 0) $
        sep [ renderPrec 1 (unDom a) <+> "->"
            , indent $ render b ]
      Pi a b               -> mparens (p > 0) $
        sep [ renderDom (domInfo a) (text (absName b) <+> ":" <+> render (unDom a)) <+> "->"
            , indent $ render (unAbs b) ]
      Sort s      -> renderPrec p s
      Level l     -> renderPrec p l
      MetaV x els -> render x `pApp` els
      DontCare v  -> renderPrec p v
      Dummy s es  -> parens (text s) `pApp` es
    where
      pApp d els = mparens (not (null els) && p > 9) $
                   sep [d, indent $ fsep (map (renderPrec 10) els)]

instance (Render t, Render e) => Render (Dom' t e) where
  render dom = pTac <+> renderDom dom (render $ unDom dom)
    where
      pTac | Just t <- domTactic dom = "@" <> parens ("tactic" <+> render t)
           | otherwise               = mempty

renderDom :: LensHiding a => a -> RichText -> RichText
renderDom i =
  case getHiding i of
    NotHidden  -> parens
    Hidden     -> braces
    Instance{} -> braces . braces

  
instance Render Clause where
  render Clause{clauseTel = tel, namedClausePats = ps, clauseBody = b, clauseType = t} =
    sep [ render tel <+> "|-"
        , indent $ sep [ fsep (map (renderPrec 10) ps) <+> "="
                       , indent $ pBody b t ] ]
    where
      pBody Nothing _ = "(absurd)"
      pBody (Just b) Nothing  = render b
      pBody (Just b) (Just t) = sep [ render b <+> ":", indent $ render t ]

instance Render a => Render (Tele (Dom a)) where
  render tel = fsep [ renderDom a (text x <+> ":" <+> render (unDom a)) | (x, a) <- telToList tel ]
    where
      telToList EmptyTel = []
      telToList (ExtendTel a tel) = (absName tel, a) : telToList (unAbs tel)

renderPrecLevelSucs :: Int -> Integer -> (Int -> RichText) -> RichText
renderPrecLevelSucs p 0 d = d p
renderPrecLevelSucs p n d = mparens (p > 9) $ "lsuc" <+> renderPrecLevelSucs 10 (n - 1) d

instance Render Level where
  renderPrec p (Max n as) =
    case as of
      []  -> renderN
      [a] | n == 0 -> renderPrec p a
      _   -> mparens (p > 9) $ List.foldr1 (\a b -> "lub" <+> a <+> b) $
        [ renderN | n > 0 ] ++ map (renderPrec 10) as
    where
      renderN = renderPrecLevelSucs p n (const "lzero")

instance Render PlusLevel where
  renderPrec p (Plus n a) = renderPrecLevelSucs p n $ \p -> renderPrec p a

instance Render LevelAtom where
  renderPrec p a =
    case a of
      MetaLevel x els  -> renderPrec p (MetaV x els)
      BlockedLevel _ v -> renderPrec p v
      NeutralLevel _ v -> renderPrec p v
      UnreducedLevel v -> renderPrec p v

instance Render Sort where
  renderPrec p s =
    case s of
      Type (ClosedLevel 0) -> "Set"
      Type (ClosedLevel n) -> text $ "Set" ++ show n
      Type l -> mparens (p > 9) $ "Set" <+> renderPrec 10 l
      Prop (ClosedLevel 0) -> "Prop"
      Prop (ClosedLevel n) -> text $ "Prop" ++ show n
      Prop l -> mparens (p > 9) $ "Prop" <+> renderPrec 10 l
      Inf -> "Setω"
      SizeUniv -> "SizeUniv"
      PiSort a b -> mparens (p > 9) $
        "piSort" <+> renderDom (domInfo a) (text (absName b) <+> ":" <+> render (unDom a))
                      <+> parens (sep [ text ("λ " ++ absName b ++ " ->")
                                      , indent $ render (unAbs b) ])
      FunSort a b -> mparens (p > 9) $
        "funSort" <+> renderPrec 10 a <+> renderPrec 10 b
      UnivSort s -> mparens (p > 9) $ "univSort" <+> renderPrec 10 s
      MetaS x es -> renderPrec p $ MetaV x es
      DefS d es  -> renderPrec p $ Def d es
      DummyS s   -> parens $ text s

instance Render Type where
  renderPrec p (El _ a) = renderPrec p a

instance Render tm => Render (Elim' tm) where
  renderPrec p (Apply v)    = renderPrec p v
  renderPrec _ (Proj _o x)  = "." <> render x
  renderPrec p (IApply x y r) = renderPrec p r

instance Render DBPatVar where
  renderPrec _ x = text $ patVarNameToString (dbPatVarName x) ++ "@" ++ show (dbPatVarIndex x)

instance Render a => Render (Pattern' a) where
  renderPrec n (VarP _o x)   = renderPrec n x
  renderPrec _ (DotP _o t)   = "." <> renderPrec 10 t
  renderPrec n (ConP c i nps)= mparens (n > 0 && not (null nps)) $
    (lazy <> render (conName c)) <+> fsep (map (renderPrec 10) ps)
    where ps = map (fmap namedThing) nps
          lazy | conPLazy i = "~"
               | otherwise  = mempty
  renderPrec n (DefP o q nps)= mparens (n > 0 && not (null nps)) $
    render q <+> fsep (map (renderPrec 10) ps)
    where ps = map (fmap namedThing) nps
  -- -- Version with printing record type:
  -- renderPrec _ (ConP c i ps) = (if b then braces else parens) $ prTy $
  --   text (show $ conName c) <+> fsep (map (render . namedArg) ps)
  --   where
  --     b = maybe False (== ConOSystem) $ conPRecord i
  --     prTy d = caseMaybe (conPType i) d $ \ t -> d  <+> ":" <+> render t
  renderPrec _ (LitP _ l)    = render l
  renderPrec _ (ProjP _o q)  = "." <> render q
  renderPrec n (IApplyP _o _ _ x) = renderPrec n x