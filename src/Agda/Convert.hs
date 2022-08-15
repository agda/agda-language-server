module Agda.Convert where

import Render ( Block(..), Inlines, renderATop, Render(..) )

import Agda.IR (FromAgda (..))
import qualified Agda.IR as IR
import Agda.Interaction.Base
import Agda.Interaction.BasicOps as B
import Agda.Interaction.EmacsCommand (Lisp)
import Agda.Interaction.Highlighting.Common (chooseHighlightingMethod, toAtoms)
import Agda.Interaction.Highlighting.Precise (Aspects (..), DefinitionSite (..), HighlightingInfo, TokenBased (..))
import qualified Agda.Interaction.Highlighting.Range as Highlighting
import Agda.Interaction.InteractionTop (localStateCommandM)
import Agda.Interaction.Response as R
import Agda.Syntax.Abstract as A
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common
import Agda.Syntax.Concrete as C
import Agda.Syntax.Internal (alwaysUnblock)
import Agda.Syntax.Position (HasRange (getRange), Range, noRange)
import Agda.Syntax.Scope.Base
import Agda.TypeChecking.Errors (getAllWarningsOfTCErr, prettyError)
import Agda.TypeChecking.Monad hiding (Function)
import Agda.TypeChecking.Monad.MetaVars (withInteractionId)
import Agda.TypeChecking.Pretty (prettyTCM)
import qualified Agda.TypeChecking.Pretty as TCP
import Agda.TypeChecking.Pretty.Warning (filterTCWarnings, prettyTCWarnings, prettyTCWarnings')
import Agda.TypeChecking.Warnings (WarningsAndNonFatalErrors (..))
import Agda.Utils.FileName (filePath)
import Agda.Utils.Function (applyWhen)
import Agda.Utils.IO.TempFile (writeToTempFile)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Maybe (catMaybes)
import Agda.Utils.Null (empty)
import Agda.Utils.Pretty hiding (render)
import Agda.Utils.RangeMap ( IsBasicRangeMap(toList) ) 
import Agda.Utils.String (delimiter)
import Agda.Utils.Time (CPUTime)
import Agda.VersionCommit (versionWithCommitInfo)
import Control.Monad.State hiding (state)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.List as List
import qualified Data.Map as Map
import Data.String (IsString)
import qualified Render

responseAbbr :: IsString a => Response -> a
responseAbbr res = case res of
  Resp_HighlightingInfo {} -> "Resp_HighlightingInfo"
  Resp_Status {} -> "Resp_Status"
  Resp_JumpToError {} -> "Resp_JumpToError"
  Resp_InteractionPoints {} -> "Resp_InteractionPoints"
  Resp_GiveAction {} -> "Resp_GiveAction"
  Resp_MakeCase {} -> "Resp_MakeCase"
  Resp_SolveAll {} -> "Resp_SolveAll"
  Resp_DisplayInfo {} -> "Resp_DisplayInfo"
  Resp_RunningInfo {} -> "Resp_RunningInfo"
  Resp_ClearRunningInfo {} -> "Resp_ClearRunningInfo"
  Resp_ClearHighlighting {} -> "Resp_ClearHighlighting"
  Resp_DoneAborting {} -> "Resp_DoneAborting"
  Resp_DoneExiting {} -> "Resp_DoneExiting"

----------------------------------

serialize :: Lisp String -> String
serialize = show . pretty

fromResponse :: Response -> TCM IR.Response
fromResponse (Resp_HighlightingInfo info remove method modFile) =
  fromHighlightingInfo info remove method modFile
fromResponse (Resp_DisplayInfo info) = IR.ResponseDisplayInfo <$> fromDisplayInfo info
fromResponse (Resp_ClearHighlighting TokenBased) = return IR.ResponseClearHighlightingTokenBased
fromResponse (Resp_ClearHighlighting NotOnlyTokenBased) = return IR.ResponseClearHighlightingNotOnlyTokenBased
fromResponse Resp_DoneAborting = return IR.ResponseDoneAborting
fromResponse Resp_DoneExiting = return IR.ResponseDoneExiting
fromResponse Resp_ClearRunningInfo = return IR.ResponseClearRunningInfo
fromResponse (Resp_RunningInfo n s) = return $ IR.ResponseRunningInfo n s
fromResponse (Resp_Status s) = return $ IR.ResponseStatus (sChecked s) (sShowImplicitArguments s)
fromResponse (Resp_JumpToError f p) = return $ IR.ResponseJumpToError f (fromIntegral p)
fromResponse (Resp_InteractionPoints is) =
  return $ IR.ResponseInteractionPoints (fmap interactionId is)
fromResponse (Resp_GiveAction (InteractionId i) giveAction) =
  return $ IR.ResponseGiveAction i (fromAgda giveAction)
fromResponse (Resp_MakeCase _ Function pcs) = return $ IR.ResponseMakeCaseFunction pcs
fromResponse (Resp_MakeCase _ ExtendedLambda pcs) = return $ IR.ResponseMakeCaseExtendedLambda pcs
fromResponse (Resp_SolveAll ps) = return $ IR.ResponseSolveAll (fmap prn ps)
  where
    prn (InteractionId i, e) = (i, prettyShow e)

fromHighlightingInfo ::
  HighlightingInfo ->
  RemoveTokenBasedHighlighting ->
  HighlightingMethod ->
  ModuleToSource ->
  TCM IR.Response
fromHighlightingInfo h remove method modFile =
  case chooseHighlightingMethod h method of
    Direct -> return $ IR.ResponseHighlightingInfoDirect info
    Indirect -> IR.ResponseHighlightingInfoIndirect <$> indirect
  where
    fromAspects ::
      (Highlighting.Range, Aspects) ->
      IR.HighlightingInfo
    fromAspects (range, aspects) =
      IR.HighlightingInfo
        (Highlighting.from range)
        (Highlighting.to range)
        (toAtoms aspects)
        (tokenBased aspects == TokenBased)
        (note aspects)
        (defSite <$> definitionSite aspects)
      where
        defSite (DefinitionSite moduleName offset _ _) =
          (filePath (Map.findWithDefault __IMPOSSIBLE__ moduleName modFile), offset)

    infos :: [IR.HighlightingInfo]
    infos = fmap fromAspects (toList h)

    keepHighlighting :: Bool
    keepHighlighting =
      case remove of
        RemoveHighlighting -> False
        KeepHighlighting -> True

    info :: IR.HighlightingInfos
    info = IR.HighlightingInfos keepHighlighting infos

    indirect :: TCM FilePath
    indirect = liftIO $ writeToTempFile (BS8.unpack (JSON.encode info))

fromDisplayInfo :: DisplayInfo -> TCM IR.DisplayInfo
fromDisplayInfo = \case 
  Info_CompilationOk _ ws -> do
    -- filter
    let filteredWarnings = filterTCWarnings (tcWarnings ws)
    let filteredErrors = filterTCWarnings (nonFatalErrors ws)
    -- serializes
    warnings <- mapM prettyTCM filteredWarnings
    errors <- mapM prettyTCM filteredErrors
    return $ IR.DisplayInfoCompilationOk (fmap show warnings) (fmap show errors)
  Info_Constraints s -> do
    -- constraints <- forM s $ \e -> do
    --   rendered <- renderTCM e
    --   let raw = show (pretty e)
    --   return $ Unlabeled rendered (Just raw)
    -- return $ IR.DisplayInfoGeneric "Constraints" constraints
    return $ IR.DisplayInfoGeneric "Constraints" [Unlabeled (Render.text $ show $ vcat $ fmap pretty s) Nothing Nothing]
  Info_AllGoalsWarnings (ims, hms) ws -> do
    -- visible metas (goals)
    goals <- mapM convertGoal ims
    -- hidden (unsolved) metas
    metas <- mapM convertHiddenMeta hms

    -- errors / warnings
    -- filter
    let filteredWarnings = filterTCWarnings (tcWarnings ws)
    let filteredErrors = filterTCWarnings (nonFatalErrors ws)
    -- serializes
    warnings <- mapM prettyTCM filteredWarnings
    errors <- mapM prettyTCM filteredErrors

    let isG = not (null goals && null metas)
    let isW = not $ null warnings
    let isE = not $ null errors
    let title =
          List.intercalate "," $
            catMaybes
              [ " Goals" <$ guard isG,
                " Errors" <$ guard isE,
                " Warnings" <$ guard isW,
                " Done" <$ guard (not (isG || isW || isE))
              ]

    return $ IR.DisplayInfoAllGoalsWarnings ("*All" ++ title ++ "*") goals metas (fmap show warnings) (fmap show errors)
    where
      convertHiddenMeta :: OutputConstraint A.Expr NamedMeta -> TCM Block
      convertHiddenMeta m = do
        let i = nmid $ namedMetaOf m
        -- output constrain
        meta <- withMetaId i $ renderATop m
        serialized <- show <$> withMetaId i (prettyATop m)
        -- range
        range <- getMetaRange i

        return $ Unlabeled meta (Just serialized) (Just range)

      convertGoal :: OutputConstraint A.Expr InteractionId -> TCM Block
      convertGoal i = do
        -- output constrain
        goal <-
          withInteractionId (outputFormId $ OutputForm noRange [] alwaysUnblock i) $
            renderATop i

        serialized <-
          withInteractionId (outputFormId $ OutputForm noRange [] alwaysUnblock i) $
            prettyATop i
        return $ Unlabeled goal (Just $ show serialized) Nothing
  Info_Auto s -> return $ IR.DisplayInfoAuto s
  Info_Error err -> do
    s <- showInfoError err
    return $ IR.DisplayInfoError s
  Info_Time s ->
    return $ IR.DisplayInfoTime (show (prettyTimed s))
  Info_NormalForm state cmode time expr -> do
    exprDoc <- evalStateT prettyExpr state
    let doc = maybe empty prettyTimed time $$ exprDoc
    return $ IR.DisplayInfoNormalForm (show doc)
    where
      prettyExpr =
        localStateCommandM $
          lift $
            B.atTopLevel $
              allowNonTerminatingReductions $
                (if computeIgnoreAbstract cmode then ignoreAbstractMode else inConcreteMode) $
                  B.showComputed cmode expr
  Info_InferredType state time expr -> do
    renderedExpr <-
      flip evalStateT state $
        localStateCommandM $
          lift $
            B.atTopLevel $
              Render.renderA expr
    let rendered = case time of
          Nothing -> renderedExpr
          -- TODO: handle this newline
          Just t -> "Time:" Render.<+> Render.render t Render.<+> "\n" Render.<+> renderedExpr
    exprDoc <-
      flip evalStateT state $
        localStateCommandM $
          lift $
            B.atTopLevel $
              TCP.prettyA expr
    let raw = show $ maybe empty prettyTimed time $$ exprDoc
    return $ IR.DisplayInfoGeneric "Inferred Type" [Unlabeled rendered (Just raw) Nothing]
  Info_ModuleContents modules tel types -> do
    doc <- localTCState $ do
      typeDocs <- addContext tel $
        forM types $ \(x, t) -> do
          doc <- prettyTCM t
          return (prettyShow x, ":" <+> doc)
      return $
        vcat
          [ "Modules",
            nest 2 $ vcat $ fmap pretty modules,
            "Names",
            nest 2 $ align 10 typeDocs
          ]
    return $ IR.DisplayInfoGeneric "Module contents" [Unlabeled (Render.text $ show doc) Nothing Nothing]
  Info_SearchAbout hits names -> do
    hitDocs <- forM hits $ \(x, t) -> do
      doc <- prettyTCM t
      return (prettyShow x, ":" <+> doc)
    let doc =
          "Definitions about"
            <+> text (List.intercalate ", " $ words names) $$ nest 2 (align 10 hitDocs)
    return $ IR.DisplayInfoGeneric "Search About" [Unlabeled (Render.text $ show doc) Nothing Nothing]
  Info_WhyInScope s cwd v xs ms -> do
    doc <- explainWhyInScope s cwd v xs ms
    return $ IR.DisplayInfoGeneric "Scope Info" [Unlabeled (Render.text $ show doc) Nothing Nothing]
  Info_Context ii ctx -> do
    doc <- localTCState (prettyResponseContexts ii False ctx)
    return $ IR.DisplayInfoGeneric "Context" [Unlabeled (Render.text $ show doc) Nothing Nothing]
  Info_Intro_NotFound ->
    return $ IR.DisplayInfoGeneric "Intro" [Unlabeled (Render.text "No introduction forms found.") Nothing Nothing]
  Info_Intro_ConstructorUnknown ss -> do
    let doc =
          sep
            [ "Don't know which constructor to introduce of",
              let mkOr [] = []
                  mkOr [x, y] = [text x <+> "or" <+> text y]
                  mkOr (x : xs) = text x : mkOr xs
               in nest 2 $ fsep $ punctuate comma (mkOr ss)
            ]
    return $ IR.DisplayInfoGeneric "Intro" [Unlabeled (Render.text $ show doc) Nothing Nothing]
  Info_Version ->
    return $ IR.DisplayInfoGeneric "Agda Version" [Unlabeled (Render.text $ "Agda version " ++ versionWithCommitInfo) Nothing Nothing]
  Info_GoalSpecific ii kind -> lispifyGoalSpecificDisplayInfo ii kind

lispifyGoalSpecificDisplayInfo :: InteractionId -> GoalDisplayInfo -> TCM IR.DisplayInfo
lispifyGoalSpecificDisplayInfo ii kind = localTCState $
  withInteractionId ii $
    case kind of
      Goal_HelperFunction helperType -> do
        doc <- inTopContext $ prettyATop helperType
        return $ IR.DisplayInfoGeneric "Helper function" [Unlabeled (Render.text $ show doc ++ "\n") Nothing Nothing]
      Goal_NormalForm cmode expr -> do
        doc <- showComputed cmode expr
        return $ IR.DisplayInfoGeneric "Normal Form" [Unlabeled (Render.text $ show doc) Nothing Nothing]
      Goal_GoalType norm aux resCtxs boundaries constraints -> do
        goalSect <- do
          (rendered, raw) <- prettyTypeOfMeta norm ii
          return [Labeled rendered (Just raw) Nothing "Goal" "special"]

        auxSect <- case aux of
          GoalOnly -> return []
          GoalAndHave expr -> do
            rendered <- renderATop expr
            raw <- show <$> prettyATop expr
            return [Labeled rendered (Just raw) Nothing "Have" "special"]
          GoalAndElaboration term -> do
            let rendered = render term
            raw <- show <$> TCP.prettyTCM term
            return [Labeled rendered (Just raw) Nothing "Elaborates to" "special"]
        let boundarySect =
              if null boundaries
                then []
                else
                  Header "Boundary" :
                  fmap (\boundary -> Unlabeled (render boundary) (Just $ show $ pretty boundary) Nothing) boundaries
        contextSect <- reverse . concat <$> mapM (renderResponseContext ii) resCtxs
        let constraintSect =
                if null constraints
                  then []
                  else
                    Header "Constraints" :
                    fmap (\constraint -> Unlabeled (render constraint) (Just $ show $ pretty constraint) Nothing) constraints

        return $
          IR.DisplayInfoGeneric "Goal type etc" $ goalSect ++ auxSect ++ boundarySect ++ contextSect ++ constraintSect
      Goal_CurrentGoal norm -> do
        (rendered, raw) <- prettyTypeOfMeta norm ii
        return $ IR.DisplayInfoCurrentGoal (Unlabeled rendered (Just raw) Nothing)
      Goal_InferredType expr -> do
        rendered <- renderATop expr
        raw <- show <$> prettyATop expr
        return $ IR.DisplayInfoInferredType (Unlabeled rendered (Just raw) Nothing)

-- -- | Format responses of DisplayInfo
-- formatPrim :: Bool -> [Block] -> String -> TCM IR.DisplayInfo
-- formatPrim _copy items header = return $ IR.DisplayInfoGeneric header items

-- -- | Format responses of DisplayInfo ("agda2-info-action")
-- format :: [Block] -> String -> TCM IR.DisplayInfo
-- format = formatPrim False

-- -- | Format responses of DisplayInfo ("agda2-info-action-copy")
-- formatAndCopy :: [Block] -> String -> TCM IR.DisplayInfo
-- formatAndCopy = formatPrim True

--------------------------------------------------------------------------------

-- | Serializing Info_Error
showInfoError :: Info_Error -> TCM String
showInfoError (Info_GenericError err) = do
  e <- prettyError err
  w <- prettyTCWarnings' =<< getAllWarningsOfTCErr err

  let errorMsg =
        if null w
          then e
          else delimiter "Error" ++ "\n" ++ e
  let warningMsg =
        List.intercalate "\n" $
          delimiter "Warning(s)" :
          filter (not . null) w
  return $
    if null w
      then errorMsg
      else errorMsg ++ "\n\n" ++ warningMsg
showInfoError (Info_CompilationError warnings) = do
  s <- prettyTCWarnings warnings
  return $
    unlines
      [ "You need to fix the following errors before you can compile",
        "the module:",
        "",
        s
      ]
showInfoError (Info_HighlightingParseError ii) =
  return $ "Highlighting failed to parse expression in " ++ show ii
showInfoError (Info_HighlightingScopeCheckError ii) =
  return $ "Highlighting failed to scope check expression in " ++ show ii

explainWhyInScope ::
  FilePath ->
  String ->
  Maybe LocalVar ->
  [AbstractName] ->
  [AbstractModule] ->
  TCM Doc
explainWhyInScope s _ Nothing [] [] = TCP.text (s ++ " is not in scope.")
explainWhyInScope s _ v xs ms =
  TCP.vcat
    [ TCP.text (s ++ " is in scope as"),
      TCP.nest 2 $ TCP.vcat [variable v xs, modules ms]
    ]
  where
    -- variable :: Maybe _ -> [_] -> TCM Doc
    variable Nothing vs = names vs
    variable (Just x) vs
      | null vs = asVar
      | otherwise =
        TCP.vcat
          [ TCP.sep [asVar, TCP.nest 2 $ shadowing x],
            TCP.nest 2 $ names vs
          ]
      where
        asVar :: TCM Doc
        asVar =
          "* a variable bound at" TCP.<+> TCP.prettyTCM (nameBindingSite $ localVar x)
        shadowing :: LocalVar -> TCM Doc
        shadowing (LocalVar _ _ []) = "shadowing"
        shadowing _ = "in conflict with"
    names = TCP.vcat . fmap pName
    modules = TCP.vcat . fmap pMod

    pKind = \case
      AxiomName                -> "postulate"
      ConName                  -> "constructor"
      CoConName                -> "coinductive constructor"
      DataName                 -> "data type"
      DisallowedGeneralizeName -> "generalizable variable from let open"
      FldName                  -> "record field"
      FunName                  -> "defined name"
      GeneralizeName           -> "generalizable variable"
      MacroName                -> "macro name"
      PatternSynName           -> "pattern synonym"
      PrimName                 -> "primitive function"
      QuotableName             -> "quotable name"
      -- previously DefName:
      RecName                  -> "record type"
      OtherDefName             -> "defined name"

    pName :: AbstractName -> TCM Doc
    pName a =
      TCP.sep
        [ "* a"
            TCP.<+> pKind (anameKind a)
            TCP.<+> TCP.text (prettyShow $ anameName a),
          TCP.nest 2 "brought into scope by"
        ]
        TCP.$$ TCP.nest 2 (pWhy (nameBindingSite $ qnameName $ anameName a) (anameLineage a))
    pMod :: AbstractModule -> TCM Doc
    pMod a =
      TCP.sep
        [ "* a module" TCP.<+> TCP.text (prettyShow $ amodName a),
          TCP.nest 2 "brought into scope by"
        ]
        TCP.$$ TCP.nest 2 (pWhy (nameBindingSite $ qnameName $ mnameToQName $ amodName a) (amodLineage a))

    pWhy :: Range -> WhyInScope -> TCM Doc
    pWhy r Defined = "- its definition at" TCP.<+> TCP.prettyTCM r
    pWhy r (Opened (C.QName x) w) | isNoName x = pWhy r w
    pWhy r (Opened m w) =
      "- the opening of"
        TCP.<+> TCP.prettyTCM m
        TCP.<+> "at"
        TCP.<+> TCP.prettyTCM (getRange m)
        TCP.$$ pWhy r w
    pWhy r (Applied m w) =
      "- the application of"
        TCP.<+> TCP.prettyTCM m
        TCP.<+> "at"
        TCP.<+> TCP.prettyTCM (getRange m)
        TCP.$$ pWhy r w

-- | Pretty-prints the context of the given meta-variable.
prettyResponseContexts ::
  -- | Context of this meta-variable.
  InteractionId ->
  -- | Print the elements in reverse order?
  Bool ->
  [ResponseContextEntry] ->
  TCM Doc
prettyResponseContexts ii rev ctxs = do
  rows <- mapM (prettyResponseContext ii) ctxs
  return $ align 10 $ concat $ applyWhen rev reverse rows

-- | Pretty-prints the context of the given meta-variable.
prettyResponseContext ::
  -- | Context of this meta-variable.
  InteractionId ->
  ResponseContextEntry ->
  TCM [(String, Doc)]
prettyResponseContext ii (ResponseContextEntry n x (Arg ai expr) letv nis) = withInteractionId ii $ do
  modality <- asksTC getModality
  do
    let prettyCtxName :: String
        prettyCtxName
          | n == x = prettyShow x
          | isInScope n == InScope = prettyShow n ++ " = " ++ prettyShow x
          | otherwise = prettyShow x

        -- Some attributes are useful to report whenever they are not
        -- in the default state.
        attribute :: String
        attribute = c ++ if null c then "" else " "
          where
            c = prettyShow (getCohesion ai)

        extras :: [Doc]
        extras =
          concat
            [ ["not in scope" | isInScope nis == C.NotInScope],
              -- Print erased if hypothesis is erased by goal is non-erased.
              ["erased" | not $ getQuantity ai `moreQuantity` getQuantity modality],
              -- Print irrelevant if hypothesis is strictly less relevant than goal.
              ["irrelevant" | not $ getRelevance ai `moreRelevant` getRelevance modality],
              -- Print instance if variable is considered by instance search
              ["instance" | isInstance ai]
            ]
    ty <- prettyATop expr

    letv' <- case letv of
      Nothing -> return []
      Just val -> do
        val' <- prettyATop val
        return [(prettyShow x, "=" <+> val')]

    return $
      (attribute ++ prettyCtxName, ":" <+> ty <+> parenSep extras) : letv'
  where
    parenSep :: [Doc] -> Doc
    parenSep docs
      | null docs = empty
      | otherwise = (" " <+>) $ parens $ fsep $ punctuate comma docs

-- | Render the context of the given meta-variable.
renderResponseContext ::
  -- | Context of this meta-variable.
  InteractionId ->
  ResponseContextEntry ->
  TCM [Block]
renderResponseContext ii (ResponseContextEntry n x (Arg ai expr) letv nis) = withInteractionId ii $ do
  modality <- asksTC getModality
  do
    let
        rawCtxName :: String
        rawCtxName
          | n == x = prettyShow x
          | isInScope n == InScope = prettyShow n ++ " = " ++ prettyShow x
          | otherwise = prettyShow x

        renderedCtxName :: Inlines
        renderedCtxName
          | n == x = render x
          | isInScope n == InScope = render n Render.<+> "=" Render.<+> render x
          | otherwise = render x

        -- Some attributes are useful to report whenever they are not
        -- in the default state.
        rawAttribute :: String
        rawAttribute = c ++ if null c then "" else " "
          where
            c = prettyShow (getCohesion ai)

        renderedAttribute :: Inlines
        renderedAttribute = c <> if null (show c) then "" else " "
          where
            c = render (getCohesion ai)

        extras :: IsString a => [a]
        extras =
          concat
            [ ["not in scope" | isInScope nis == C.NotInScope],
              -- Print erased if hypothesis is erased by goal is non-erased.
              ["erased" | not $ getQuantity ai `moreQuantity` getQuantity modality],
              -- Print irrelevant if hypothesis is strictly less relevant than goal.
              ["irrelevant" | not $ getRelevance ai `moreRelevant` getRelevance modality],
              -- Print instance if variable is considered by instance search
              ["instance" | isInstance ai]
            ]

        extras2 :: [Inlines]
        extras2 =
          concat
            [ ["not in scope" | isInScope nis == C.NotInScope],
              -- Print erased if hypothesis is erased by goal is non-erased.
              ["erased" | not $ getQuantity ai `moreQuantity` getQuantity modality],
              -- Print irrelevant if hypothesis is strictly less relevant than goal.
              ["irrelevant" | not $ getRelevance ai `moreRelevant` getRelevance modality],
              -- Print instance if variable is considered by instance search
              ["instance" | isInstance ai]
            ]

    -- raw
    rawExpr <- prettyATop expr
    let rawType = show $ align 10 [(rawAttribute ++ rawCtxName, ":" <+> rawExpr <+> parenSep extras)]
    -- rendered  
    renderedExpr <- renderATop expr
    let renderedType = (renderedCtxName <> renderedAttribute) Render.<+> ":" Render.<+> renderedExpr Render.<+> parenSep2 extras2
      -- (Render.fsep $ Render.punctuate "," extras)

    -- result 
    let typeItem = Unlabeled renderedType (Just rawType) Nothing

    valueItem <- case letv of
      Nothing -> return []
      Just val -> do
        valText <- renderATop val
        valString <- prettyATop val
        let renderedValue = Render.render x Render.<+> "=" Render.<+> valText
        let rawValue = show $ align 10 [(prettyShow x, "=" <+> valString)]
        return
          [ Unlabeled renderedValue (Just rawValue) Nothing
          ]

    return $ typeItem : valueItem
  where
    parenSep :: [Doc] -> Doc
    parenSep docs
      | null docs = empty
      | otherwise = (" " <+>) $ parens $ fsep $ punctuate comma docs

    parenSep2 :: [Inlines] -> Inlines
    parenSep2 docs
      | null docs = mempty
      | otherwise = (" " Render.<+>) $ Render.parens $ Render.fsep $ Render.punctuate "," docs


-- | Pretty-prints the type of the meta-variable.
prettyTypeOfMeta :: Rewrite -> InteractionId -> TCM (Inlines, String)
prettyTypeOfMeta norm ii = do
  form <- B.typeOfMeta norm ii
  case form of
    OfType _ e -> do
      rendered <- renderATop e
      raw <- show <$> prettyATop e
      return (rendered, raw)
    _ -> do
      rendered <- renderATop form
      raw <- show <$> prettyATop form
      return (rendered, raw)

-- | Prefix prettified CPUTime with "Time:"
prettyTimed :: CPUTime -> Doc
prettyTimed time = "Time:" <+> pretty time
