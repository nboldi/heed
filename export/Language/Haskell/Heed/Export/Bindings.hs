{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import HsBinds
import Bag
import BasicTypes
import HsTypes
import HsExpr hiding (Match)
import qualified HsExpr as GHC
import SrcLoc

exportBinding :: HsName n => Exporter (Located (HsBind n))
exportBinding (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _)) =
  export FunctionB l [ mapM_ exportMatch matches ]

-- trfBind' :: TransformName n r => HsBind n -> Trf (AST.UValueBind (Dom r) RangeStage)
-- -- A value binding with a strcitness annotation
-- trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = L _ [L _ (Match { m_ctxt = FunRhs { mc_strictness = SrcStrict }, m_pats = [], m_grhss = GRHSs [L _ (GRHS [] expr)] (L _ locals) })]} })
--   = do bangLoc <- focusBeforeLoc (srcSpanStart $ getLoc id) $ tokenLoc AnnBang
--        AST.USimpleBind <$> annLocNoSema (pure $ combineSrcSpans bangLoc (getLoc id))
--                              (AST.UBangPat <$> copyAnnot AST.UVarPat (define $ trfName id))
--                        <*> addEmptyScope (addToScope locals (annLocNoSema (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UUnguardedRhs <$> trfExpr expr)))
--                        <*> addEmptyScope (trfWhereLocalBinds (getLoc expr) locals)
-- -- A value binding (not a function)
-- trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = L _ [L _ (Match { m_pats = [], m_grhss = GRHSs [L _ (GRHS [] expr)] (L _ locals) })]} })
--   = AST.USimpleBind <$> copyAnnot AST.UVarPat (define $ trfName id)
--                     <*> addEmptyScope (addToScope locals (annLocNoSema (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UUnguardedRhs <$> trfExpr expr)))
--                     <*> addEmptyScope (trfWhereLocalBinds (getLoc expr) locals)
-- trfBind' (FunBind id (MG (unLoc -> matches) _ _ _) _ _ _)
--   = AST.UFunBind <$> makeNonemptyIndentedList (mapM (trfMatch (unLoc id)) matches)
-- trfBind' (PatBind pat (GRHSs rhs (unLoc -> locals)) _ _ _)
--   = AST.USimpleBind <$> trfPattern pat
--                     <*> addEmptyScope (addToScope locals (trfRhss rhs))
--                     <*> addEmptyScope (trfWhereLocalBinds (collectLocs rhs) locals)
-- trfBind' (PatSynBind _) = convertionProblem "Pattern synonym bindings should be recognized on the declaration level"
-- trfBind' b = unhandledElement "binding" b
--

exportMatch :: forall n . HsName n => Exporter (Located (GHC.Match n (LHsExpr n)))
exportMatch (L l (GHC.Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert Match l
  defining $ goInto id 1
    $ exportNameOrRdrName @n (case mc_fixity name of BasicTypes.Prefix -> exportName
                                                     BasicTypes.Infix -> exportOperator)
                             (mc_fun name)
  addToScope (combineLocated pats) $ do
   goInto id 2 $ mapM_ exportPattern pats
   goInto id 3 $ mapM_ exportRhss rhss

exportAlternative :: forall n . HsName n => Exporter (Located (GHC.Match n (LHsExpr n)))
exportAlternative (L l (GHC.Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert Alternative l
  addToScope (combineLocated pats) $ do
   goInto id 1 $ mapM_ exportPattern pats
   goInto id 2 $ mapM_ exportRhss rhss


exportRhss :: HsName n => Exporter (Located (GRHS n (LHsExpr n)))
exportRhss (L l (GRHS [] body)) =
  export Unguarded l [ exportExpression body ]

exportCaseRhss :: HsName n => Exporter (Located (GRHS n (LHsExpr n)))
exportCaseRhss (L l (GRHS [] body)) =
  export UnguardedC l [ exportExpression body ]

exportLocalBinds :: HsName n => Exporter (LHsLocalBinds n)
exportLocalBinds (L l (HsValBinds (ValBindsIn binds sigs)))
  = export LocalBindings l [ mapM_ exportBinding (bagToList binds) >> mapM_ exportLocalSig sigs ]
exportLocalBinds (L l (HsValBinds (ValBindsOut binds sigs)))
  = export LocalBindings l [ mapM_ exportBinding (concatMap (bagToList . snd) binds) >> mapM_ exportLocalSig sigs ]
-- trfLocalBinds token (HsIPBinds (IPBinds binds _))
--   = makeIndentedListBefore " " (after token) (mapM trfIpBind binds)
exportLocalBinds (L l lb) = exportError "local binds" lb


exportLocalSig :: HsName n => Exporter (Located (Sig n))
exportLocalSig ts@(L l (TypeSig {})) = export LocalTypeSignature l [exportTypeSignature ts]
exportLocalSig (L l (FixSig fs)) = export LocalFixitySignature l [exportFixitySignature (L l fs)]
-- exportLocalSig (L l (InlineSig name prag)) = export LocalInline l (exportInlinePragma name prag)
exportLocalSig (L l ls) = exportError "local signature" ls

exportTypeSignature :: HsName n => Exporter (Located (Sig n))
exportTypeSignature (L l (TypeSig names typ))
  = export TypeSignature l [ mapM_ exportName names, exportType (hsib_body $ hswc_body typ) ]
exportTypeSignature (L l ts) = exportError "type signature" ts


exportFixitySignature :: HsName n => Exporter (Located (FixitySig n))
exportFixitySignature (L l (FixitySig names (Fixity _ prec InfixL)))
  = export FixitySignatureLeft l [ writeIntAttribute prec ]
exportFixitySignature (L l (FixitySig names (Fixity _ prec InfixR)))
  = export FixitySignatureRight l [ writeIntAttribute prec ]
exportFixitySignature (L l (FixitySig names (Fixity _ prec InfixN)))
  = export FixitySignature l [ writeIntAttribute prec ]



-- trfMatch :: TransformName n r => n -> Located (Match n (LHsExpr n)) -> Trf (Ann AST.UMatch (Dom r) RangeStage)
-- trfMatch id = trfLocNoSema (trfMatch' id)
--
-- trfMatch' :: TransformName n r => n -> Match n (LHsExpr n) -> Trf (AST.UMatch (Dom r) RangeStage)
-- trfMatch' name (Match funid pats _ (GRHSs rhss (unLoc -> locBinds)))
--   -- TODO: add the optional typ to pats
--   = AST.UMatch <$> trfMatchLhs name funid pats
--                <*> addToScope pats (addToScope locBinds (trfRhss rhss))
--                <*> addToScope pats (trfWhereLocalBinds (collectLocs rhss) locBinds)
--
-- trfMatchLhs :: TransformName n r => n -> HsMatchContext (NameOrRdrName n) -> [LPat n] -> Trf (Ann AST.UMatchLhs (Dom r) RangeStage)
-- trfMatchLhs name fb pats
--   = do implicitIdLoc <- mkSrcSpan <$> atTheStart <*> atTheStart
--        parenOpLoc <- tokensLoc [AnnOpenP, AnnVal, AnnCloseP]
--        nonFunOpLoc <- tokenLoc AnnVal
--        let infixLoc = case (parenOpLoc, nonFunOpLoc) of
--                         (RealSrcSpan rsp1, RealSrcSpan rsp2)
--                           | srcLocCol (realSrcSpanStart rsp2) == srcLocCol (realSrcSpanStart rsp1) + 1
--                               && srcLocCol (realSrcSpanEnd rsp2) == srcLocCol (realSrcSpanEnd rsp1) - 1 -> parenOpLoc
--                         _ -> nonFunOpLoc -- sometimes parenOpLoc is not an actual operator in parentheses, it just grabs
--                                          -- a paren, so we need to check that it is actually what we seek
--        closeLoc <- srcSpanStart <$> (combineSrcSpans <$> tokenLoc AnnEqual <*> tokenLoc AnnVbar)
--        args <- mapM trfPattern pats
--        let (n, isInfix) = case fb of FunRhs n inf _ -> (n, inf == Infix)
--                                      _ -> let token = if isSymOcc (occName name) && isGoodSrcSpan infixLoc then infixLoc else implicitIdLoc
--                                            in (L token name, length pats > 0 && srcSpanStart token >= srcSpanEnd (getLoc (pats !! 0)))
--        annLocNoSema (mkSrcSpan <$> atTheStart <*> (pure closeLoc)) $
--         case (args, isInfix) of
--            (left:right:rest, True) -> AST.UInfixLhs left <$> define (trfOperator n) <*> pure right <*> makeList " " (pure closeLoc) (pure rest)
--            _                       -> AST.UNormalLhs <$> define (trfName n) <*> makeList " " (pure closeLoc) (pure args)
--
-- trfRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.URhs (Dom r) RangeStage)
-- -- the original location on the GRHS misleadingly contains the local bindings
-- trfRhss [unLoc -> GRHS [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> tokenBefore (srcSpanStart $ getLoc body) AnnEqual)
--                                          (AST.UUnguardedRhs <$> trfExpr body)
-- trfRhss rhss = annLocNoSema (pure $ collectLocs rhss)
--                       (AST.UGuardedRhss . nonemptyAnnList <$> mapM trfGuardedRhs rhss)
--
-- trfGuardedRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.UGuardedRhs (Dom r) RangeStage)
-- trfGuardedRhs = trfLocNoSema $ \(GRHS guards body)
--   -> AST.UGuardedRhs . nonemptyAnnList <$> trfScopedSequence trfRhsGuard guards <*> addToScope guards (trfExpr body)
--
-- trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.URhsGuard (Dom r) RangeStage)
-- trfRhsGuard = trfLocNoSema trfRhsGuard'
--
-- trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.URhsGuard (Dom r) RangeStage)
-- trfRhsGuard' (BindStmt pat body _ _ _) = AST.UGuardBind <$> trfPattern pat <*> trfExpr body
-- trfRhsGuard' (BodyStmt body _ _ _) = AST.UGuardCheck <$> trfExpr body
-- trfRhsGuard' (LetStmt (unLoc -> binds)) = AST.UGuardLet <$> trfLocalBinds AnnLet binds
-- trfRhsGuard' d = unhandledElement "guard" d
--
-- trfWhereLocalBinds :: TransformName n r => SrcSpan -> HsLocalBinds n -> Trf (AnnMaybeG AST.ULocalBinds (Dom r) RangeStage)
-- trfWhereLocalBinds _ EmptyLocalBinds = nothing "" "" atTheEnd
-- trfWhereLocalBinds bef binds
--   = makeJust <$> annLocNoSema (combineSrcSpans (srcLocSpan (srcSpanEnd bef) `combineSrcSpans` getBindLocs binds) <$> tokenLocBack AnnWhere)
--                               (AST.ULocalBinds <$> addToScope binds (trfLocalBinds AnnWhere binds))
--
-- getBindLocs :: HsLocalBinds n -> SrcSpan
-- getBindLocs (HsValBinds (ValBindsIn binds sigs)) = foldLocs $ map getLoc (bagToList binds) ++ map getLoc sigs
-- getBindLocs (HsValBinds (ValBindsOut binds sigs)) = foldLocs $ map getLoc (concatMap (bagToList . snd) binds) ++ map getLoc sigs
-- getBindLocs (HsIPBinds (IPBinds binds _)) = foldLocs $ map getLoc binds
-- getBindLocs EmptyLocalBinds = noSrcSpan

-- trfIpBind :: TransformName n r => Located (IPBind n) -> Trf (Ann AST.ULocalBind (Dom r) RangeStage)
-- trfIpBind = trfLocNoSema $ \case
--   IPBind (Left (L l ipname)) expr
--     -> AST.ULocalValBind
--          <$> (annContNoSema $ AST.USimpleBind <$> focusOn l (annContNoSema (AST.UVarPat <$> define (trfImplicitName ipname)))
--                                               <*> annFromNoSema AnnEqual (AST.UUnguardedRhs <$> trfExpr expr)
--                                               <*> nothing " " "" atTheEnd)
--   IPBind (Right _) _ -> convertionProblem "trfIpBind: called on typechecked AST"
--
--

-- trfInlinePragma :: TransformName n r => Located n -> InlinePragma -> Trf (Ann AST.UInlinePragma (Dom r) RangeStage)
-- trfInlinePragma name (InlinePragma _ Inlinable _ phase _)
--   = annContNoSema (AST.UInlinablePragma <$> trfPhase (pure $ srcSpanStart $ getLoc name) phase <*> trfName name)
-- trfInlinePragma name (InlinePragma _ NoInline _ _ _) = annContNoSema (AST.UNoInlinePragma <$> trfName name)
-- trfInlinePragma name (InlinePragma (fromSrcText -> src) Inline _ phase cl)
--   = annContNoSema $ do rng <- asks contRange
--                        let parts = map getLoc $ splitLocated (L rng src)
--                        AST.UInlinePragma <$> trfConlike parts cl
--                                          <*> trfPhase (pure $ srcSpanStart (getLoc name)) phase
--                                          <*> trfName name
--
-- trfPhase :: Trf SrcLoc -> Activation -> Trf (AnnMaybeG AST.UPhaseControl (Dom r) RangeStage)
-- trfPhase l AlwaysActive = nothing " " "" l
-- trfPhase _ (ActiveAfter _ pn) = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS)
--                                                           (AST.UPhaseControl <$> nothing "" "" (before AnnCloseS) <*> (makeJust <$> trfPhaseNum pn))
-- trfPhase _ (ActiveBefore _ pn) = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS)
--                                                            (AST.UPhaseControl <$> (makeJust <$> annLocNoSema (tokenLoc AnnTilde) (pure AST.PhaseInvert)) <*> (makeJust <$> trfPhaseNum pn))
-- trfPhase _ NeverActive = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS)
--                                                    (AST.UPhaseControl <$> (makeJust <$> annLocNoSema (tokenLoc AnnTilde) (pure AST.PhaseInvert)) <*> nothing " " "" (after AnnTilde))
--
-- trfPhaseNum ::  PhaseNum -> Trf (Ann AST.PhaseNumber (Dom r) RangeStage)
-- trfPhaseNum i = annLocNoSema (tokenLoc AnnVal) $ pure (AST.PhaseNumber $ fromIntegral i)
--
-- trfConlike :: [SrcSpan] -> RuleMatchInfo -> Trf (AnnMaybeG AST.UConlikeAnnot (Dom r) RangeStage)
-- trfConlike parts ConLike | length parts > 2
--   = makeJust <$> annLocNoSema (pure $ parts !! 2) (pure AST.UConlikeAnnot)
--   | otherwise = convertionProblem $ "trfConlike: expected 3 parts, got: " ++ show parts
-- trfConlike (_:inlTok:_) FunLike = nothing " " "" (pure $ srcSpanEnd inlTok)
-- trfConlike (combTok:_) FunLike = nothing " " "" (pure $ srcSpanEnd combTok)



-- trfAlt :: TransformName n r => Located (Match n (LHsExpr n)) -> Trf (Ann AST.UAlt (Dom r) RangeStage)
-- trfAlt = trfLocNoSema trfAlt'
--
-- trfAlt' :: TransformName n r => Match n (LHsExpr n) -> Trf (AST.UAlt (Dom r) RangeStage)
-- trfAlt' = gTrfAlt' trfExpr
--
-- gTrfAlt' :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> Match n (Located (ge n)) -> Trf (AST.UAlt' ae (Dom r) RangeStage)
-- gTrfAlt' te (Match _ [pat] _ (GRHSs rhss (unLoc -> locBinds)))
--   = AST.UAlt <$> trfPattern pat <*> gTrfCaseRhss te rhss <*> trfWhereLocalBinds (collectLocs rhss) locBinds
-- gTrfAlt' _ _ = convertionProblem "gTrfAlt': not exactly one alternative when transforming a case alternative"
--
-- trfCaseRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.UCaseRhs (Dom r) RangeStage)
-- trfCaseRhss = gTrfCaseRhss trfExpr
--
-- gTrfCaseRhss :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> [Located (GRHS n (Located (ge n)))] -> Trf (Ann (AST.UCaseRhs' ae) (Dom r) RangeStage)
-- gTrfCaseRhss te [unLoc -> GRHS [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> updateFocus (pure . updateEnd (const $ srcSpanStart $ getLoc body))
--                                                                                                       (tokenLocBack AnnRarrow))
--                                                  (AST.UUnguardedCaseRhs <$> te body)
-- gTrfCaseRhss te rhss = annLocNoSema (pure $ collectLocs rhss)
--                               (AST.UGuardedCaseRhss <$> trfAnnList ";" (gTrfGuardedCaseRhs' te) rhss)
--
-- trfGuardedCaseRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.UGuardedCaseRhs (Dom r) RangeStage)
-- trfGuardedCaseRhs = trfLocNoSema trfGuardedCaseRhs'
--
-- trfGuardedCaseRhs' :: TransformName n r => GRHS n (LHsExpr n) -> Trf (AST.UGuardedCaseRhs (Dom r) RangeStage)
-- trfGuardedCaseRhs' = gTrfGuardedCaseRhs' trfExpr
--
-- gTrfGuardedCaseRhs' :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> GRHS n (Located (ge n)) -> Trf (AST.UGuardedCaseRhs' ae (Dom r) RangeStage)
-- gTrfGuardedCaseRhs' te (GRHS guards body) = AST.UGuardedCaseRhs <$> trfAnnList " " trfRhsGuard' guards <*> te body

