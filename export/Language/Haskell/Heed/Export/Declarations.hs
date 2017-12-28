{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Declarations where

import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import HsDecls
import HsDecls
import GHC
import Bag
import HsSyn
import SrcLoc

exportDeclarationGroup :: HsName n => Exporter (HsGroup n)
exportDeclarationGroup g@(HsGroup vals splices tycls derivs fixities defaults foreigns warns anns rules vects _)
  = addToScope (combineLocated allDecls)
      $ goInto Nothing 1 $ mapM_ exportDeclaration allDecls
  where (sigs, bagToList -> binds) = getBindsAndSigs vals
        allDecls = (map (fmap SpliceD) splices)
                      ++ (map (fmap ValD) binds)
                      ++ (map (fmap SigD) sigs)
                      ++ (map (fmap TyClD) (concat $ map group_tyclds tycls))
                      ++ (map (fmap DerivD) derivs)
                      ++ (map (fmap (SigD . FixSig)) fixities)
                      ++ (map (fmap DefD) defaults)
                      ++ (map (fmap ForD) foreigns)
                      ++ (map (fmap WarningD) warns)
                      ++ (map (fmap AnnD) anns)
                      ++ (map (fmap RuleD) rules)
                      ++ (map (fmap VectD) vects)
                      ++ (map (fmap InstD) (hsGroupInstDecls g))

exportDeclaration :: HsName n => Exporter (Located (HsDecl n))
exportDeclaration (L l (ValD bind)) = export BindingD l [ exportBinding (L l bind) ]
exportDeclaration (L l (TyClD (SynDecl name vars _ rhs _)))
  = export TypeSynonymD l [ exportName name, mapM_ exportTypeVar (hsq_explicit vars), exportType rhs ]
exportDeclaration (L l (TyClD (DataDecl name vars _ (HsDataDefn nd ctx _ kind cons derivs) _ _)))
  | all (isNormalCons . unLoc) cons
  = export DataD l [ exportName name
                   , mapM_ exportTypeVar (hsq_explicit vars)
                   , exportContext ctx
                   , mapM_ exportConstructor cons
                   , exportDerivings derivs
                   ]
  where isNormalCons ConDeclH98{} = True
        isNormalCons _ = False

-- trfGADT :: TransformName n r => NewOrData -> Located n -> LHsQTyVars n -> Located (HsContext n)
--                                  -> Maybe (Located (HsKind n)) -> [Located (ConDecl n)]
--                                  -> Located [LHsDerivingClause n] -> AnnKeywordId -> Trf SrcLoc -> Trf (AST.UDecl (Dom r) RangeStage)
-- trfGADT nd name vars ctx kind cons derivs ctxTok consLoc
--   = AST.UGDataDecl <$> trfDataKeyword nd
--                    <*> trfCtx (after ctxTok) ctx
--                    <*> betweenIfPresent ctxTok AnnEqual (createDeclHead name vars)
--                    <*> focusBefore AnnWhere (trfKindSig kind)
--                    <*> makeIndentedListBefore " where " consLoc (mapM trfGADTConDecl cons)
--                    <*> makeIndentedList atTheEnd (mapM trfDerivings (unLoc derivs))


--   TyClD (ClassDecl ctx name vars _ funDeps sigs defs typeFuns typeFunDefs _ _)
--     -> AST.UClassDecl <$> trfCtx (after AnnClass) ctx
--                      <*> betweenIfPresent AnnClass AnnWhere (createDeclHead name vars)
--                      <*> trfFunDeps funDeps
--                      <*> createClassBody sigs defs typeFuns typeFunDefs
--   InstD (ClsInstD (ClsInstDecl typ binds sigs typefam datafam overlap))
--     -> AST.UInstDecl <$> trfMaybeDefault " " "" trfOverlap (after AnnInstance) overlap
--                     <*> trfInstanceRule (hsib_body typ)
--                     <*> trfInstBody binds sigs typefam datafam

-- exportDeclaration (L l (TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars _ kindSig inj))))
--   = export ClosedFamilyD l [ exportName name
--                            , defining $ mapM_ exportTypeVar tyVars
--                            , exportKindSig kindsSig
--                            , exportInjectivity inj
--                            , exportTypeEquations typeEqs
--                            ]


-- trfDecl :: TransformName n r => Located (HsDecl n) -> Trf (Ann AST.UDecl (Dom r) RangeStage)
-- trfDecl = trfLocNoSema $ \case
--   TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars _ kindSig inj))
--     -> AST.UClosedTypeFamilyDecl <$> focusAfter AnnType (createDeclHead name tyVars)
--                                 <*> trfFamilyResultSig kindSig inj
--                                 <*> trfTypeEqs typeEqs
--   TyClD (FamDecl fd) -> AST.UTypeFamilyDecl <$> annContNoSema (trfTypeFam' fd)

--   InstD (DataFamInstD (DataFamInstDecl con pats _ (HsDataDefn nd _ _ _ cons derivs) _))
--     | all ((\case ConDeclH98{} -> True; _ -> False) . unLoc) cons
--     -> AST.UDataInstDecl <$> trfDataKeyword nd
--                         <*> (focusAfter AnnInstance . focusBeforeIfPresent AnnEqual . focusBeforeIfPresent AnnDeriving)
--                               (makeInstanceRuleTyVars con pats)
--                                                        -- the location is needed when there is no = sign
--                         <*> makeListBefore " = " " | " (pure $ srcSpanStart $ foldLocs $ getLoc con : map getLoc (hsib_body pats)) (mapM trfConDecl cons)
--                         <*> makeIndentedList atTheEnd (mapM trfDerivings (unLoc derivs))
--   InstD (DataFamInstD (DataFamInstDecl con pats _ (HsDataDefn nd _ _ kind cons _) _))
--     -> AST.UGDataInstDecl <$> trfDataKeyword nd
--                         <*> (focusAfter AnnInstance . focusBeforeIfPresent AnnWhere)
--                               (makeInstanceRuleTyVars con pats)
--                         <*> focusBefore AnnWhere (trfKindSig kind)
--                         <*> makeIndentedListBefore " where " atTheEnd (mapM trfGADTConDecl cons)
--   InstD (TyFamInstD (TyFamInstDecl (L _ (TyFamEqn con pats _ rhs)) _))
--     -> AST.UTypeInstDecl <$> between AnnInstance AnnEqual (makeInstanceRuleTyVars con pats) <*> trfType rhs
--   ValD bind -> trfVal bind
--   SigD sig -> trfSig sig
--   DerivD (DerivDecl t strat overlap) -> AST.UDerivDecl <$> trfDerivingStrategy strat <*> trfMaybeDefault " " "" trfOverlap (after AnnInstance) overlap <*> trfInstanceRule (hsib_body t)
--   RuleD (HsRules _ rules) -> AST.UPragmaDecl <$> annContNoSema (AST.URulePragma <$> makeIndentedList (before AnnClose) (mapM trfRewriteRule rules))
--   RoleAnnotD (RoleAnnotDecl name roles) -> AST.URoleDecl <$> trfQualifiedName False name <*> makeList " " atTheEnd (mapM trfRole roles)
--   DefD (DefaultDecl types) -> AST.UDefaultDecl <$> makeList "," (after AnnOpenP) (mapM trfType types)
--   ForD (ForeignImport name (hsib_body -> typ) _ (CImport ccall safe _ _ _))
--     -> AST.UForeignImport <$> trfCallConv ccall <*> trfSafety (getLoc ccall) safe <*> define (trfName name) <*> trfType typ
--   ForD (ForeignExport name (hsib_body -> typ) _ (CExport (L l (CExportStatic _ _ ccall)) _))
--     -> AST.UForeignExport <$> annLocNoSema (pure l) (trfCallConv' ccall) <*> trfName name <*> trfType typ
--   SpliceD (SpliceDecl (unLoc -> spl) _) -> AST.USpliceDecl <$> trfSplice spl
--   WarningD (Warnings _ [])
--     -> AST.UPragmaDecl <$> annContNoSema (AST.UDeprPragma <$> (makeList " " (after AnnOpen) (pure []))
--                                                           <*> makeList ", " (before AnnClose) (pure []))
--   WarningD (Warnings _ [L _ (Warning names (DeprecatedTxt _ stringLits))])
--     -> AST.UPragmaDecl <$> annContNoSema (AST.UDeprPragma <$> (makeList " " (after AnnOpen) $ mapM trfName names)
--                                                           <*> makeList ", " (before AnnClose) (mapM (\(L l (StringLiteral _ fs)) -> trfFastString (L l fs)) stringLits))
--   WarningD (Warnings _ [L _ (Warning names (WarningTxt _ stringLits))])
--     -> AST.UPragmaDecl <$> annContNoSema (AST.UWarningPragma <$> (makeNonemptyList " " $ mapM trfName names)
--                                                              <*> makeList ", " (before AnnClose) (mapM (\(L l (StringLiteral _ fs)) -> trfFastString (L l fs)) stringLits))
--   AnnD (HsAnnotation stxt subject expr)
--     -> AST.UPragmaDecl <$> annContNoSema (AST.UAnnPragma <$> trfAnnotationSubject stxt subject (srcSpanStart $ getLoc expr) <*> trfExpr expr)
--   d -> unhandledElement "declaration" d
--

-- trfVal :: TransformName n r => HsBindLR n n -> Trf (AST.UDecl (Dom r) RangeStage)
-- trfVal (PatSynBind psb) = AST.UPatternSynonymDecl <$> annContNoSema (trfPatternSynonym psb)
-- trfVal bind = AST.UValueBinding <$> (annContNoSema $ trfBind' bind)
--
-- trfSig :: TransformName n r => Sig n -> Trf (AST.UDecl (Dom r) RangeStage)
-- trfSig (ts @ (TypeSig {})) = AST.UTypeSigDecl <$> defineTypeVars (annContNoSema $ trfTypeSig' ts)
-- trfSig (FixSig fs) = AST.UFixityDecl <$> (annContNoSema $ trfFixitySig fs)
-- trfSig (PatSynSig ids typ)
--   = AST.UPatTypeSigDecl <$> annContNoSema (AST.UPatternTypeSignature <$> trfAnnList ", " trfName' ids <*> trfType (hsib_body typ))
-- trfSig (InlineSig name prag)
--   = AST.UPragmaDecl <$> annContNoSema (AST.UInlinePragmaDecl <$> trfInlinePragma name prag)
-- trfSig (SpecSig name (map hsib_body -> types) (inl_act -> phase))
--   = AST.UPragmaDecl <$> annContNoSema (AST.USpecializeDecl <$> trfSpecializePragma name types phase)
-- trfSig (CompleteMatchSig _ names typeConstraint)
--   = AST.UPragmaDecl <$> annContNoSema (AST.UCompletePragma <$> trfAnnList ", " trfName' (unLoc names)
--                                                            <*> trfMaybe " :: " "" trfName typeConstraint)
-- trfSig s = unhandledElement "signature" s
--
-- trfSpecializePragma :: TransformName n r
--                     => Located n -> [Located (HsType n)] -> Activation -> Trf (Ann AST.USpecializePragma (Dom r) RangeStage)
-- trfSpecializePragma name types phase
--   = annContNoSema $ AST.USpecializePragma <$> trfPhase (pure $ srcSpanStart (getLoc name)) phase
--                                           <*> trfName name
--                                           <*> (orderAnnList <$> trfAnnList ", " trfType' types)
--

exportConstructor :: HsName n => Exporter (Located (ConDecl n))
exportConstructor (L l (ConDeclH98 { con_name = name, con_qvars = tyVars, con_cxt = ctx
                                   , con_details = PrefixCon args }))
  = export Constructor l [ mapM_ exportTypeVar (maybe [] hsq_explicit tyVars), maybe (return ()) exportContext ctx
                         , defining (exportName name), mapM_ exportType args ]
exportConstructor (L l (ConDeclH98 { con_name = name, con_qvars = tyVars, con_cxt = ctx
                                   , con_details = RecCon (unLoc -> flds) }))
  = export RecordConstructor l [ mapM_ exportTypeVar (maybe [] hsq_explicit tyVars), maybe (return ()) exportContext ctx
                               , defining (exportName name), mapM_ exportFieldDecl flds ]
exportConstructor (L l (ConDeclH98 { con_name = name, con_qvars = tyVars, con_cxt = ctx
                                   , con_details = InfixCon t1 t2 }))
  = export RecordConstructor l [ mapM_ exportTypeVar (maybe [] hsq_explicit tyVars), maybe (return ()) exportContext ctx
                               , exportType t1, defining (exportName name), exportType t2 ]

--
-- trfConTyVars :: TransformName n r => Maybe (LHsQTyVars n) -> Trf (AnnListG AST.UTyVar (Dom r) RangeStage)
-- trfConTyVars Nothing = makeListAfter "." " " atTheStart (return [])
-- trfConTyVars (Just vars) = trfBindings $ hsq_explicit vars
--
-- trfConCtx :: TransformName n r => Maybe (LHsContext n) -> Trf (AnnMaybeG AST.UContext (Dom r) RangeStage)
-- trfConCtx Nothing = nothing "" " => " atTheStart
-- trfConCtx (Just ctx) = trfCtx atTheStart ctx
--
-- trfGADTConDecl :: TransformName n r => Located (ConDecl n) -> Trf (Ann AST.UGadtConDecl (Dom r) RangeStage)
-- trfGADTConDecl = trfLocNoSema trfGADTConDecl'
--
-- trfGADTConDecl' :: TransformName n r => ConDecl n -> Trf (AST.UGadtConDecl (Dom r) RangeStage)
-- trfGADTConDecl' (ConDeclGADT { con_names = names, con_type = hsib_body -> typ })
--   = let nameLoc = collectLocs names
--         typLoc = getLoc typ
--         (vars, ctx, t) = getTypeVarsAndCtx typ
--      in AST.UGadtConDecl <$> define (trfAnnList ", " trfName' names)
--                          <*> focusOn (mkSrcSpan (srcSpanEnd nameLoc) (srcSpanStart typLoc)) (trfBindings vars)
--                          <*> updateFocus (return . updateEnd (\_ -> srcSpanStart typLoc)) (focusAfterIfPresent AnnDot (trfCtx atTheStart ctx))
--                          <*> trfGadtConType t
--   where getTypeVarsAndCtx :: LHsType n -> ([LHsTyVarBndr n], LHsContext n, LHsType n)
--         getTypeVarsAndCtx (L _ (HsForAllTy [] typ)) = getTypeVarsAndCtx typ
--         getTypeVarsAndCtx (L _ (HsForAllTy bndrs typ)) = let (_,ctx,t) = getTypeVarsAndCtx typ in (bndrs, ctx, t)
--         getTypeVarsAndCtx (L _ (HsQualTy ctx typ)) = let (vars,_,t) = getTypeVarsAndCtx typ in (vars, ctx, t)
--         getTypeVarsAndCtx t = ([], L noSrcSpan [], t)
--
-- trfGadtConType :: TransformName n r => Located (HsType n) -> Trf (Ann AST.UGadtConType (Dom r) RangeStage)
-- trfGadtConType = trfLocNoSema $ \case
--   HsFunTy (cleanHsType . unLoc -> HsRecTy flds) resType
--     -> AST.UGadtRecordType <$> between AnnOpenC AnnCloseC (trfAnnList ", " trfFieldDecl' flds)
--                            <*> trfType resType
--   typ -> AST.UGadtNormalType <$> annContNoSema (trfType' typ)
--

exportFieldDecl :: HsName n => Exporter (Located (ConDeclField n))
exportFieldDecl (L l (ConDeclField names typ _))
  = export FieldDecl l [ defining (mapM_ exportFieldOccName names), exportType typ ]

exportDerivings :: HsName n => Exporter (Located [LHsDerivingClause n])
exportDerivings = undefined

-- trfDerivings :: TransformName n r => Located (HsDerivingClause n) -> Trf (Ann AST.UDeriving (Dom r) RangeStage)
-- trfDerivings = trfLocNoSema $ \case
--   HsDerivingClause strat (unLoc->[hsib_body -> typ@(unLoc -> HsTyVar {})])
--     -> AST.UDerivingOne <$> trfDerivingStrategy strat <*> trfInstanceHead typ
--   HsDerivingClause strat derivs
--     -> AST.UDerivings <$> trfDerivingStrategy strat <*> focusOn (getLoc derivs) (trfAnnList ", " trfInstanceHead' (map hsib_body (unLoc derivs)))
--
-- trfDerivingStrategy :: Maybe (Located DerivStrategy) -> Trf (AnnMaybeG AST.UDeriveStrategy (Dom r) RangeStage)
-- trfDerivingStrategy = trfMaybeDefault " " ""
--                         (trfLocNoSema $ \case StockStrategy -> return AST.UStockStrategy
--                                               AnyclassStrategy -> return AST.UAnyClassStrategy
--                                               NewtypeStrategy -> return AST.UNewtypeStrategy)
--                         atTheStart
--
-- trfInstanceRule :: TransformName n r => Located (HsType n) -> Trf (Ann AST.UInstanceRule (Dom r) RangeStage)
-- trfInstanceRule = trfLocNoSema (trfInstanceRule' . cleanHsType)
--
-- trfInstanceRule' :: TransformName n r => HsType n -> Trf (AST.UInstanceRule (Dom r) RangeStage)
-- trfInstanceRule' (HsForAllTy bndrs (unLoc -> HsQualTy ctx typ))
--   = AST.UInstanceRule <$> (makeJust <$> annLocNoSema (pure $ collectLocs bndrs) (trfBindings bndrs))
--                       <*> trfCtx (after AnnDot) ctx
--                       <*> trfInstanceHead typ
-- trfInstanceRule' (HsQualTy ctx typ) = AST.UInstanceRule <$> nothing "" " . " atTheStart
--                                                         <*> trfCtx atTheStart ctx
--                                                         <*> trfInstanceHead typ
-- trfInstanceRule' (HsParTy typ) = instanceHead $ annContNoSema (AST.UInstanceHeadParen <$> trfInstanceHead typ)
-- trfInstanceRule' (HsTyVar _ tv) = instanceHead $ annContNoSema (AST.UInstanceHeadCon <$> trfName tv)
-- trfInstanceRule' (HsAppTy t1 t2) = instanceHead $ annContNoSema (AST.UInstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2)
-- trfInstanceRule' (HsOpTy t1 op t2) = instanceHead $ annContNoSema (AST.UInstanceHeadApp <$> annLocNoSema (pure $ getLoc t1 `combineSrcSpans` getLoc op) (AST.UInstanceHeadInfix <$> trfType t1 <*> trfOperator op) <*> trfType t2)
-- trfInstanceRule' t = unhandledElement "instance rule" t
--
-- instanceHead :: Trf (Ann AST.UInstanceHead (Dom r) RangeStage) -> Trf (AST.UInstanceRule (Dom r) RangeStage)
-- instanceHead hd = AST.UInstanceRule <$> (nothing "" " . " atTheStart) <*> (nothing " " "" atTheStart) <*> hd
--
-- makeInstanceRuleTyVars :: TransformName n r => Located n -> HsImplicitBndrs n [LHsType n] -> Trf (Ann AST.UInstanceRule (Dom r) RangeStage)
-- makeInstanceRuleTyVars n vars
--   | isSymOcc (occName (unLoc n))
--   , leftOp:rest <- hsib_body vars
--   , srcSpanStart (getLoc n) > srcSpanEnd (getLoc leftOp)
--   = annContNoSema
--       $ AST.UInstanceRule <$> nothing "" " . " atTheStart
--                           <*> nothing " " "" atTheStart
--                           <*> foldl foldTypeArgs
--                                     (annLocNoSema (pure $ combineSrcSpans (getLoc leftOp) (getLoc n))
--                                       (AST.UInstanceHeadInfix <$> trfType leftOp <*> trfOperator n)) rest
--   | otherwise
--   = annContNoSema
--       $ AST.UInstanceRule <$> nothing "" " . " atTheStart
--                           <*> nothing " " "" atTheStart
--                           <*> foldl foldTypeArgs (copyAnnot AST.UInstanceHeadCon (trfName n)) (hsib_body vars)
--   where foldTypeArgs base typ = annLocNoSema (pure $ combineSrcSpans (getLoc n) (getLoc typ)) $ AST.UInstanceHeadApp <$> base <*> (trfType typ)
--
--
-- trfInstanceHead :: TransformName n r => Located (HsType n) -> Trf (Ann AST.UInstanceHead (Dom r) RangeStage)
-- trfInstanceHead = trfLocNoSema trfInstanceHead'
--
-- trfInstanceHead' :: TransformName n r => HsType n -> Trf (AST.UInstanceHead (Dom r) RangeStage)
-- trfInstanceHead' = trfInstanceHead'' . cleanHsType where
--   trfInstanceHead'' (HsForAllTy [] (unLoc -> t)) = trfInstanceHead' t
--   trfInstanceHead'' (HsTyVar _ tv) = AST.UInstanceHeadCon <$> trfName tv
--   trfInstanceHead'' (HsAppTy t1 t2) = AST.UInstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2
--   trfInstanceHead'' (HsParTy typ) = AST.UInstanceHeadParen <$> trfInstanceHead typ
--   trfInstanceHead'' (HsOpTy t1 op t2)
--     = AST.UInstanceHeadApp <$> (annLocNoSema (pure $ combineSrcSpans (getLoc t1) (getLoc op))
--                                              (AST.UInstanceHeadInfix <$> trfType t1 <*> trfOperator op))
--                           <*> trfType t2
--   trfInstanceHead'' t = unhandledElement "instance head" t
--
-- trfTypeEqs :: TransformName n r => Maybe [Located (TyFamInstEqn n)] -> Trf (AnnListG AST.UTypeEqn (Dom r) RangeStage)
-- trfTypeEqs eqs =
--   do toks <- tokensAfter AnnWhere
--      case toks of [] -> convertionProblem "trfTypeEqs: no where found after closed type family"
--                   loc:_ -> makeList "\n" (pure $ srcSpanStart loc) (mapM trfTypeEq (fromMaybe [] eqs))
--
-- trfTypeEq :: TransformName n r => Located (TyFamInstEqn n) -> Trf (Ann AST.UTypeEqn (Dom r) RangeStage)
-- trfTypeEq = trfLocNoSema $ \(TyFamEqn name pats _ rhs)
--   -> AST.UTypeEqn <$> defineTypeVars (focusBefore AnnEqual (combineTypes name (hsib_body pats))) <*> trfType rhs
--   where combineTypes :: TransformName n r => Located n -> [LHsType n] -> Trf (Ann AST.UType (Dom r) RangeStage)
--         combineTypes name [lhs, rhs] | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs)
--           = annContNoSema $ AST.UTyInfix <$> trfType lhs <*> trfOperator name <*> trfType rhs
--         combineTypes name pats = wrapTypes (annLocNoSema (pure $ getLoc name) (AST.UTyVar <$> trfName name)) pats
--
--         wrapTypes :: TransformName n r => Trf (Ann AST.UType (Dom r) RangeStage) -> [LHsType n] -> Trf (Ann AST.UType (Dom r) RangeStage)
--         wrapTypes base pats
--           = foldl (\t p -> do typ <- t
--                               annLocNoSema (pure $ combineSrcSpans (getRange typ) (getLoc p))
--                                      (AST.UTyApp <$> pure typ <*> trfType p)) base pats
--
-- trfFunDeps :: TransformName n r => [Located (FunDep (Located n))] -> Trf (AnnMaybeG AST.UFunDeps (Dom r) RangeStage)
-- trfFunDeps [] = do whereToken <- tokenLoc AnnWhere
--                    nothing "| " "" (if isGoodSrcSpan whereToken then pure $ srcSpanStart whereToken else atTheEnd)
-- trfFunDeps fundeps = makeJust <$> annLocNoSema (combineSrcSpans (collectLocs fundeps) <$> tokenLoc AnnVbar)
--                                          (AST.UFunDeps <$> trfAnnList ", " trfFunDep' fundeps)
--
-- trfFunDep' :: TransformName n r => FunDep (Located n) -> Trf (AST.UFunDep (Dom r) RangeStage)
-- trfFunDep' (lhs, rhs) = AST.UFunDep <$> trfAnnList ", " trfName' lhs <*> trfAnnList ", " trfName' rhs
--
-- createDeclHead :: TransformName n r => Located n -> LHsQTyVars n -> Trf (Ann AST.UDeclHead (Dom r) RangeStage)
-- createDeclHead name (hsq_explicit -> lhs : rhs : rest)
--   | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs)
--   -- infix declaration
--   = wrapDeclHead rest
--       $ annLocNoSema (addParenLocs $ getLoc lhs `combineSrcSpans` getLoc rhs)
--                      (AST.UDHInfix <$> defineTypeVars (trfTyVar lhs) <*> define (trfOperator name) <*> defineTypeVars (trfTyVar rhs))
-- createDeclHead name vars = defineTypeVars $ wrapDeclHead (hsq_explicit vars) (define $ copyAnnot AST.UDeclHead (trfName name))
--
-- wrapDeclHead :: TransformName n r => [LHsTyVarBndr n] -> Trf (Ann AST.UDeclHead (Dom r) RangeStage) -> Trf (Ann AST.UDeclHead (Dom r) RangeStage)
-- wrapDeclHead vars base
--   = foldl (\t p -> do typ <- t
--                       annLocNoSema (addParenLocs $ combineSrcSpans (getRange typ) (getLoc p))
--                              (AST.UDHApp typ <$> trfTyVar p)
--           ) base vars
--
-- -- | Get the parentheses directly before and after (for parenthesized application)
-- addParenLocs :: SrcSpan -> Trf SrcSpan
-- addParenLocs sp
--   = let possibleSpan = mkSrcSpan (updateCol (subtract 1) (srcSpanStart sp)) (updateCol (+1) (srcSpanEnd sp))
--      in local (\s -> s { contRange = possibleSpan })
--               (combineSrcSpans <$> (combineSrcSpans sp <$> tokenLoc AnnOpenP) <*> tokenLocBack AnnCloseP)
--
--
-- createClassBody :: TransformName n r => [LSig n] -> LHsBinds n -> [LFamilyDecl n]
--                                -> [LTyFamDefltEqn n] -> Trf (AnnMaybeG AST.UClassBody (Dom r) RangeStage)
-- createClassBody sigs binds typeFams typeFamDefs
--   = do isThereWhere <- isGoodSrcSpan <$> (tokenLoc AnnWhere)
--        if isThereWhere
--          then makeJust <$> annLocNoSema (combinedLoc <$> tokenLoc AnnWhere)
--                                         (AST.UClassBody <$> makeList "" (after AnnWhere)
--                                                                        (orderDefs . concat <$> sequenceA allDefs))
--          else nothing " where " "" atTheEnd
--   where combinedLoc wh = foldl combineSrcSpans wh allLocs
--         allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc typeFams ++ map getLoc typeFamDefs
--         allDefs = [getSigs, getBinds, getFams, getFamDefs]
--         getSigs = mapM trfClassElemSig sigs
--         getBinds = mapM (copyAnnot AST.UClsDef . trfBind) (bagToList binds)
--         getFams = mapM (copyAnnot AST.UClsTypeFam . trfTypeFam) typeFams
--         getFamDefs = mapM trfTypeFamDef typeFamDefs
--
-- trfClassElemSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.UClassElement (Dom r) RangeStage)
-- trfClassElemSig = trfLocNoSema $ \case
--   TypeSig names typ -> AST.UClsSig <$> (annContNoSema $ AST.UTypeSignature <$> define (makeNonemptyList ", " (mapM trfName names))
--                                   <*> trfType (hsib_body $ hswc_body typ))
--   ClassOpSig True [name] typ -> AST.UClsDefSig <$> trfName name <*> trfType (hsib_body typ)
--   ClassOpSig False names typ -> AST.UClsSig <$> (annContNoSema $ AST.UTypeSignature <$> define (makeNonemptyList ", " (mapM trfName names))
--                                            <*> trfType (hsib_body typ))
--   MinimalSig _ formula -> AST.UClsMinimal <$> trfMinimalFormula formula
--   InlineSig name prag -> AST.UClsInline <$> trfInlinePragma name prag
--   FixSig fixity -> AST.UClsFixity <$> annContNoSema (trfFixitySig fixity)
--   s -> unhandledElement "signature in class" s
--
-- trfTypeFam :: TransformName n r => Located (FamilyDecl n) -> Trf (Ann AST.UTypeFamily (Dom r) RangeStage)
-- trfTypeFam = trfLocNoSema trfTypeFam'
--
-- trfTypeFam' :: TransformName n r => FamilyDecl n -> Trf (AST.UTypeFamily (Dom r) RangeStage)
-- trfTypeFam' (FamilyDecl DataFamily name tyVars _ kindSig _)
--   = AST.UDataFamily <$> (case unLoc kindSig of KindSig _ -> between AnnData AnnDcolon; _ -> id) (createDeclHead name tyVars)
--                     <*> trfFamilyKind kindSig
-- trfTypeFam' (FamilyDecl OpenTypeFamily name tyVars _ kindSig injectivity)
--   = AST.UTypeFamily <$> (case unLoc kindSig of KindSig _ -> between AnnType AnnDcolon; _ -> id) (createDeclHead name tyVars)
--                    <*> trfFamilyResultSig kindSig injectivity
-- trfTypeFam' (FamilyDecl (ClosedTypeFamily {}) _ _ _ _ _) = convertionProblem "trfTypeFam': closed type family received"
--
-- trfTypeFamDef :: TransformName n r => Located (TyFamDefltEqn n) -> Trf (Ann AST.UClassElement (Dom r) RangeStage)
-- trfTypeFamDef = trfLocNoSema $ \(TyFamEqn con pats _ rhs)
--   -> AST.UClsTypeDef <$> between AnnType AnnEqual (createDeclHead con pats) <*> trfType rhs
--
-- trfInstBody :: TransformName n r => LHsBinds n -> [LSig n] -> [LTyFamInstDecl n] -> [LDataFamInstDecl n] -> Trf (AnnMaybeG AST.UInstBody (Dom r) RangeStage)
-- trfInstBody binds sigs fams dats = do
--     wh <- tokenLoc AnnWhere
--     if isGoodSrcSpan wh then
--       makeJust <$> annLocNoSema (combinedLoc <$> tokenLoc AnnWhere)
--                                 (AST.UInstBody <$> (makeList "" (after AnnWhere)
--                                                       (orderDefs . concat <$> sequenceA allDefs)))
--     else nothing " where " "" atTheEnd
--   where combinedLoc wh = foldl combineSrcSpans wh allLocs
--         allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc fams ++ map getLoc dats
--         allDefs = [getSigs, getBinds, getFams, getDats]
--         getSigs = mapM trfClassInstSig sigs
--         getBinds = mapM (copyAnnot AST.UInstBodyNormalDecl . trfBind) (bagToList binds)
--         getFams = mapM trfInstTypeFam fams
--         getDats = mapM trfInstDataFam dats
--
-- trfClassInstSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.UInstBodyDecl (Dom r) RangeStage)
-- trfClassInstSig = trfLocNoSema $ \case
--   TypeSig names typ -> AST.UInstBodyTypeSig <$> (annContNoSema $ AST.UTypeSignature <$> makeNonemptyList ", " (mapM trfName names)
--                                            <*> trfType (hsib_body $ hswc_body typ))
--   ClassOpSig _ names typ -> AST.UInstBodyTypeSig <$> (annContNoSema $ AST.UTypeSignature <$> define (makeNonemptyList ", " (mapM trfName names))
--                                                 <*> trfType (hsib_body typ))
--   SpecInstSig _ typ -> AST.USpecializeInstance <$> trfType (hsib_body typ)
--   SpecSig name (map hsib_body -> tys) (inl_act -> phase) -> AST.UInstanceSpecialize <$> trfSpecializePragma name tys phase
--   InlineSig name prag -> AST.UInlineInstance <$> trfInlinePragma name prag
--   s -> unhandledElement "class instance signature" s
--
-- trfInstTypeFam :: TransformName n r => Located (TyFamInstDecl n) -> Trf (Ann AST.UInstBodyDecl (Dom r) RangeStage)
-- trfInstTypeFam (unLoc -> TyFamInstDecl eqn _) = copyAnnot AST.UInstBodyTypeDecl (trfTypeEq eqn)
--
-- trfInstDataFam :: TransformName n r => Located (DataFamInstDecl n) -> Trf (Ann AST.UInstBodyDecl (Dom r) RangeStage)
-- trfInstDataFam = trfLocNoSema $ \case
--   (DataFamInstDecl tc (hsib_body -> pats) _ (HsDataDefn dn ctx _ ks cons derivs) _)
--     | all ((\case ConDeclH98{} -> True; _ -> False) . unLoc) cons
--     -> AST.UInstBodyDataDecl
--          <$> trfDataKeyword dn
--          <*> annLocNoSema (pure $ collectLocs pats `combineSrcSpans` getLoc tc `combineSrcSpans` getLoc ctx)
--                           (AST.UInstanceRule <$> nothing "" " . " atTheStart
--                                              <*> trfCtx atTheStart ctx
--                                              <*> transformNameAndPats tc pats)
--          <*> trfAnnList "" trfConDecl' cons
--          <*> makeIndentedList atTheEnd (mapM trfDerivings (unLoc derivs))
--     | otherwise
--     -> AST.UInstBodyGadtDataDecl
--         <$> trfDataKeyword dn
--         <*> annLocNoSema (pure $ collectLocs pats `combineSrcSpans` getLoc tc `combineSrcSpans` getLoc ctx)
--                          (AST.UInstanceRule <$> nothing "" " . " atTheStart
--                                             <*> trfCtx atTheStart ctx
--                                             <*> transformNameAndPats tc pats)
--         <*> trfKindSig ks
--         <*> trfAnnList "" trfGADTConDecl' cons
--         <*> makeIndentedList atTheEnd (mapM trfDerivings (unLoc derivs))
--   where transformNameAndPats tc pats -- TODO: this is simpler with lexical fixity
--           | all (\p -> srcSpanEnd (getLoc tc) < srcSpanStart (getLoc p)) pats -- prefix instance head application
--           = foldl (\r t -> annLocNoSema (combineSrcSpans (getLoc t) . getRange <$> r)
--                                           (AST.UInstanceHeadApp <$> r <*> (trfType t)))
--                   (copyAnnot AST.UInstanceHeadCon (trfName tc)) pats
--         transformNameAndPats tc (p:rest)
--           | otherwise -- infix instance head application
--           = foldl (\r t -> annLocNoSema (combineSrcSpans (getLoc t) . getRange <$> r)
--                                           (AST.UInstanceHeadApp <$> r <*> (trfType t)))
--                   (annLocNoSema (pure $ getLoc p `combineSrcSpans` getLoc tc)
--                           (AST.UInstanceHeadInfix <$> trfType p <*> trfOperator tc)) rest
--
-- trfPatternSynonym :: forall n r . TransformName n r => PatSynBind n n -> Trf (AST.UPatternSynonym (Dom r) RangeStage)
-- trfPatternSynonym (PSB id _ lhs def dir)
--   = let sep = case dir of ImplicitBidirectional -> AnnEqual
--                           _                     -> AnnLarrow
--         rhsLoc = combineSrcSpans (getLoc def) <$> tokenLoc sep
--         -- we use the selector name instead of the pattern variable name
--         rewrites = case lhs of RecordPatSyn flds -> map (\r -> (unLoc (recordPatSynPatVar r), unLoc (recordPatSynSelectorId r))) flds
--                                _                 -> []
--         changedRhs = biplateRef .- (\n -> case lookup n rewrites of Just x -> x; Nothing -> n) $ def
--      in AST.UPatternSynonym <$> trfPatSynLhs id lhs sep
--                             <*> annLocNoSema rhsLoc (trfPatSynRhs dir changedRhs)
--
--   where trfPatSynLhs :: Located n -> HsPatSynDetails (Located n) -> AnnKeywordId -> Trf (Ann AST.UPatSynLhs (Dom r) RangeStage)
--         trfPatSynLhs id (PrefixPatSyn args) _
--           = annLocNoSema (pure $ foldLocs (getLoc id : map getLoc args)) $ AST.UNormalPatSyn <$> define (trfName id) <*> trfAnnList " " trfName' args
--         trfPatSynLhs op (InfixPatSyn lhs rhs) _
--           = annLocNoSema (pure $ getLoc lhs `combineSrcSpans` getLoc rhs) $ AST.UInfixPatSyn <$> define (trfName lhs) <*> trfOperator op <*> trfName rhs
--         trfPatSynLhs id (RecordPatSyn flds) kw
--           = annLocNoSema (mkSrcSpan (srcSpanStart (getLoc id)) <$> before kw)
--               $ AST.URecordPatSyn <$> define (trfName id) <*> trfAnnList ", " trfName' (map recordPatSynSelectorId flds)
--
--         trfPatSynRhs :: HsPatSynDir n -> Located (Pat n) -> Trf (AST.UPatSynRhs (Dom r) RangeStage)
--         trfPatSynRhs ImplicitBidirectional pat = AST.UBidirectionalPatSyn <$> trfPattern pat <*> nothing " where " "" atTheEnd
--         trfPatSynRhs (ExplicitBidirectional mg) pat = AST.UBidirectionalPatSyn <$> trfPattern pat <*> (makeJust <$> trfPatSynWhere mg)
--         trfPatSynRhs Unidirectional pat = AST.UOneDirectionalPatSyn <$> trfPattern pat
--
--         trfPatSynWhere :: MatchGroup n (LHsExpr n) -> Trf (Ann AST.UPatSynWhere (Dom r) RangeStage)
--         trfPatSynWhere (MG { mg_alts = alts }) = annLocNoSema (pure $ getLoc alts) (AST.UPatSynWhere <$> makeIndentedList (after AnnWhere) (mapM (trfMatch (unLoc id)) (unLoc alts)))
--
-- trfFamilyKind :: TransformName n r => Located (FamilyResultSig n) -> Trf (AnnMaybeG AST.UKindConstraint (Dom r) RangeStage)
-- trfFamilyKind (unLoc -> fr) = case fr of
--   NoSig -> nothing "" " " atTheEnd
--   KindSig k -> trfKindSig (Just k)
--   TyVarSig _ -> convertionProblem "trfFamilyKind: TyVarSig not supported"
--
-- trfFamilyResultSig :: TransformName n r => Located (FamilyResultSig n) -> Maybe (LInjectivityAnn n) -> Trf (AnnMaybeG AST.UTypeFamilySpec (Dom r) RangeStage)
-- trfFamilyResultSig (L l fr) Nothing = case fr of
--   NoSig -> nothing "" " " atTheEnd
--   KindSig k -> makeJust <$> (annLocNoSema (pure l) $ AST.UTypeFamilyKind <$> trfKindSig' k)
--   TyVarSig tv -> makeJust <$> (annLocNoSema (pure l) $ AST.UTypeFamilyTyVar <$> trfTyVar tv)
-- trfFamilyResultSig (L _ sig) (Just (L l (InjectivityAnn n deps)))
--   = makeJust <$> (annLocNoSema (pure l) $ AST.UTypeFamilyInjectivity <$> (annContNoSema $ AST.UInjectivityAnn <$> tv <*> trfAnnList ", " trfName' deps))
--     where tv = case sig of TyVarSig tv -> trfTyVar tv
--                            _ -> annLocNoSema (pure $ getLoc n) (AST.UTyVarDecl <$> trfName n <*> nothing "" "" (pure $ srcSpanEnd (getLoc n)))
--
-- trfAnnotationSubject :: TransformName n r => SourceText -> AnnProvenance n -> SrcLoc -> Trf (Ann AST.UAnnotationSubject (Dom r) RangeStage)
-- trfAnnotationSubject (fromSrcText -> stxt) subject payloadEnd
--   = do payloadStart <- advanceStr stxt <$> atTheStart
--        case subject of ValueAnnProvenance name@(L l _) -> annLocNoSema (pure l) (AST.UNameAnnotation <$> trfName name)
--                        TypeAnnProvenance name@(L l _) -> annLocNoSema (pure $ mkSrcSpan payloadStart (srcSpanEnd l))
--                                                                       (AST.UTypeAnnotation <$> trfName name)
--                        ModuleAnnProvenance -> annLocNoSema (pure $ mkSrcSpan payloadStart payloadEnd) (pure AST.UModuleAnnotation)
--
-- trfDataKeyword ::  NewOrData -> Trf (Ann AST.UDataOrNewtypeKeyword (Dom r) RangeStage)
-- trfDataKeyword NewType = annLocNoSema (tokenLoc AnnNewtype) (pure AST.UNewtypeKeyword)
-- trfDataKeyword DataType = annLocNoSema (tokenLoc AnnData) (pure AST.UDataKeyword)
--
-- trfCallConv :: Located CCallConv -> Trf (Ann AST.UCallConv (Dom r) RangeStage)
-- trfCallConv = trfLocNoSema trfCallConv'
--
-- trfCallConv' :: CCallConv -> Trf (AST.UCallConv (Dom r) RangeStage)
-- trfCallConv' CCallConv = pure AST.UCCall
-- trfCallConv' CApiConv = pure AST.UCApi
-- trfCallConv' StdCallConv = pure AST.UStdCall
-- trfCallConv' JavaScriptCallConv = pure AST.UJavaScript
-- trfCallConv' PrimCallConv = convertionProblem "trfCallConv: PrimCallConv not supported"
--
-- trfSafety :: SrcSpan -> Located Safety -> Trf (AnnMaybeG AST.USafety (Dom r) RangeStage)
-- trfSafety ccLoc lsaf@(L l _) | isGoodSrcSpan l
--   = makeJust <$> trfLocNoSema (pure . \case
--       PlaySafe -> AST.USafe
--       PlayInterruptible -> AST.UInterruptible
--       PlayRisky -> AST.UUnsafe) lsaf
--   | otherwise = nothing " " "" (pure $ srcSpanEnd ccLoc)
--
-- trfOverlap :: Located OverlapMode -> Trf (Ann AST.UOverlapPragma (Dom r) RangeStage)
-- trfOverlap = trfLocNoSema $ pure . \case
--   NoOverlap _ -> AST.UDisableOverlap
--   Overlappable _ -> AST.UOverlappable
--   Overlapping _ -> AST.UOverlapping
--   Overlaps _ -> AST.UOverlaps
--   Incoherent _ -> AST.UIncoherentOverlap
--
-- trfRole :: Located (Maybe Role) -> Trf (Ann AST.URole (Dom r) RangeStage)
-- trfRole = trfLocNoSema $ \case Just Nominal -> pure AST.UNominal
--                                Just Representational -> pure AST.URepresentational
--                                Just GHC.Phantom -> pure AST.UPhantom
--                                Nothing -> convertionProblem "trfRole: no role"
--
-- trfRewriteRule :: TransformName n r => Located (RuleDecl n) -> Trf (Ann AST.URule (Dom r) RangeStage)
-- trfRewriteRule = trfLocNoSema $ \(HsRule (L nameLoc (_, ruleName)) act bndrs left _ right _) ->
--   AST.URule <$> trfFastString (L nameLoc ruleName)
--             <*> trfPhase (pure $ srcSpanEnd nameLoc) act
--             <*> makeListAfter " " " " (pure $ srcSpanStart $ getLoc left) (mapM trfRuleBndr bndrs)
--             <*> trfExpr left
--             <*> trfExpr right
--
-- trfRuleBndr :: TransformName n r => Located (RuleBndr n) -> Trf (Ann AST.URuleVar (Dom r) RangeStage)
-- trfRuleBndr = trfLocNoSema $ \case (RuleBndr n) -> AST.URuleVar <$> trfName n
--                                    (RuleBndrSig n k) -> AST.USigRuleVar <$> trfName n <*> trfType (hsib_body $ hswc_body k)
--
-- trfMinimalFormula :: TransformName n r => Located (BooleanFormula (Located n)) -> Trf (Ann AST.UMinimalFormula (Dom r) RangeStage)
-- trfMinimalFormula = trfLocCorrect (pure mkNoSemanticInfo)
--                       (\sp -> if isGoodSrcSpan sp then pure sp else srcLocSpan <$> before AnnClose) trfMinimalFormula'
--
-- trfMinimalFormula' :: TransformName n r => BooleanFormula (Located n) -> Trf (AST.UMinimalFormula (Dom r) RangeStage)
-- trfMinimalFormula' (Var name) = AST.UMinimalName <$> trfName name
-- trfMinimalFormula' (And formulas) -- empty Minimal pragma is mapped to an empty list
--   = AST.UMinimalAnd <$> makeListBefore " " " , " atTheEnd (mapM (trfLocNoSema trfMinimalFormula') formulas)
-- trfMinimalFormula' (Or formulas) = AST.UMinimalOr <$> trfAnnList " | " trfMinimalFormula' formulas
-- trfMinimalFormula' (Parens formula) = AST.UMinimalParen <$> trfMinimalFormula formula
