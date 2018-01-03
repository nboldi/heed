{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Declarations where

import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Types
import {-# SOURCE #-} Language.Haskell.Heed.Export.Templates
import Language.Haskell.Heed.Export.Kinds
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema as Schema

import Data.Data
import HsDecls
import FastString
import Data.Maybe
import ForeignCall as GHC
import BasicTypes as GHC
import BooleanFormula
import GHC
import Bag
import TyCon as GHC
import HsSyn
import SrcLoc
import Class

import Control.Monad.IO.Class

exportDeclarationGroup :: HsName n => Exporter (HsGroup n)
exportDeclarationGroup g@(HsGroup vals splices tycls derivs fixities defaults foreigns warns anns rules vects _)
  = addToScope (combineLocated allDecls) $ mapM_ exportDeclaration allDecls
  where (sigs, bagToList -> binds) = getBindsAndSigs vals
        allDecls = (map (fmap GHC.SpliceD) splices)
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
exportDeclaration (L l (ValD (PatSynBind (PSB id _ lhs def dir))))
  = export PatternSynonymD l [ exportPatternSynonymLhs id lhs, exportPatternSynonymRhs dir def ]
exportDeclaration (L l (ValD bind)) = export BindingD l [ exportBinding (L l bind) ]
exportDeclaration (L l (SigD ts@(TypeSig {})))
  = export TypeSignatureD l [ exportTypeSignature (L l ts) ]
exportDeclaration (L l (SigD (PatSynSig ids typ)))
  = export PatternSignatureD l [ mapM_ exportName ids, exportType (hsib_body typ) ]
exportDeclaration (L l (SigD (FixSig fs)))
  = export FixitySignatureD l [ exportFixitySignature (L l fs) ]
exportDeclaration (L l (SigD (InlineSig name prag)))
  = export PragmaD l [ export Schema.InlinePragma l [exportName name, exportInlinePragma (L l prag)] ]
exportDeclaration (L l (SigD (SpecSig name (map hsib_body -> types) (inl_act -> phase))))
  = export PragmaD l [ export SpecializePragma l [exportPhase (L l phase), exportName name, mapM_ exportType types] ]
exportDeclaration (L l (SigD (CompleteMatchSig _ (L _ names) typeConstraint)))
  = export PragmaD l [ export CompletePragma l [mapM_ exportName names, maybe (return ()) exportName typeConstraint] ]
exportDeclaration (L l (SigD s)) = exportError "declaration (signature)" s

exportDeclaration (L l (TyClD (SynDecl name vars _ rhs _)))
  = export TypeSynonymD l [ exportName name, mapM_ exportTypeVar (hsq_explicit vars), exportType rhs ]
exportDeclaration (L l (TyClD (DataDecl name vars _ (HsDataDefn nd ctx _ kind cons derivs) _ _)))
  | all (isNormalCons . unLoc) cons
  = export DataD l [ exportDataKeyword (L l nd)
                   , exportDeclHead name vars
                   , exportContext ctx
                   , mapM_ exportConstructor cons
                   , mapM_ exportDerivings (unLoc derivs)
                   ]
  | otherwise
  = export GDataD l [ exportDataKeyword (L l nd)
                    , exportDeclHead name vars
                    , exportContext ctx
                    , exportKindSignature kind
                    , mapM_ exportGADTConstructor cons
                    , mapM_ exportDerivings (unLoc derivs)
                    ]
  where isNormalCons ConDeclH98{} = True
        isNormalCons _ = False
exportDeclaration (L l (TyClD (ClassDecl ctx name vars _ funDeps sigs defs typeFuns typeFunDefs _ _)))
  = export ClassD l [ exportContext ctx, exportDeclHead name vars, mapM_ exportFunDep funDeps
                    , do mapM_ exportClassElemSig sigs
                         mapM_ (\(L l b) -> export ClassBinding l [exportBinding (L l b)]) (bagToList defs)
                         mapM_ (\(L l b) -> export ClassTypeFamily l [exportTypeFamily (L l b)]) typeFuns
                         mapM_ exportTypeFamilyDefault typeFunDefs
                    ]
exportDeclaration (L l (InstD (ClsInstD (ClsInstDecl typ binds sigs typefam datafam overlap))))
  = export InstanceD l [ maybe (return ()) exportOverlap overlap
                       , exportInstanceRule (hsib_body typ)
                       , do mapM_ exportClassInstSigs sigs
                            mapM_ (\(L l b) -> export InstanceBinding l [exportBinding (L l b)]) (bagToList binds)
                            mapM_ exportInstanceTypeFamily typefam
                            mapM_ exportInstanceDataFamily datafam
                       ]
exportDeclaration (L l (TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars _ kindSig inj))))
  = export ClosedFamilyD l [ exportName name
                           , defining $ mapM_ exportTypeVar (hsq_explicit tyVars)
                           , exportFamilyResSig inj kindSig
                           , mapM_ exportTypeEquation (fromMaybe [] typeEqs)
                           ]
exportDeclaration (L l (TyClD (FamDecl fd))) = export TypeFamilyD l [ exportTypeFamily (L l fd) ]

exportDeclaration (L l (InstD (DataFamInstD df)))
  = export DataInstanceD l [ exportInstanceDataFamily (L l df) ]
exportDeclaration (L l (InstD (TyFamInstD tf)))
  = export TypeInstanceD l [ exportInstanceTypeFamily (L l tf) ]
exportDeclaration (L l (DerivD (DerivDecl t strat overlap)))
  = export DerivingD l [ exportDerivingStrategy strat
                       , maybe (return ()) exportOverlap overlap
                       , exportInstanceRule (hsib_body t)
                       ]
exportDeclaration (L l (RuleD (HsRules _ rules)))
  = export PragmaD l [ export RulePragma l [ mapM_ exportRewriteRule rules ] ]
exportDeclaration (L l (RoleAnnotD (RoleAnnotDecl name roles)))
  = export RoleD l [ exportName name, mapM_ exportRole roles ]
exportDeclaration (L l (DefD (DefaultDecl types)))
  = export DefaultD l [ mapM_ exportType types ]
exportDeclaration (L l (ForD (ForeignImport name (hsib_body -> typ) _ (CImport ccall safe _ _ _))))
  = export ForeignImportD l [ exportCallConvention ccall, exportSafety safe, defining (exportName name), exportType typ ]
exportDeclaration (L l (ForD (ForeignExport name (hsib_body -> typ) _ (CExport (L lc (CExportStatic _ _ ccall)) _))))
  = export ForeignExportD l [ exportCallConvention (L lc ccall), exportName name, exportType typ ]
exportDeclaration (L l (GHC.SpliceD (SpliceDecl spl _))) = export Schema.SpliceD l [ exportSplice spl ]
exportDeclaration (L l (WarningD _)) = return () -- TODO: warnings
exportDeclaration (L l (AnnD _)) = return () -- TODO: annotations
exportDeclaration (L l d) = exportError "declaration" d

exportDeclHead :: HsName n => Located n -> Exporter (LHsQTyVars n)
exportDeclHead name (hsq_explicit -> lhs : rhs : rest)
  | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs) -- infix declaration
  = defining $ export InfixDH (combineLocated [lhs,rhs])
                 [ exportTypeVar lhs, exportOperator name, exportTypeVar rhs, mapM_ exportTypeVar rest ]
exportDeclHead name vars
  = defining $ export PrefixDH (combineSpans (getLoc name : map getLoc (hsq_explicit vars)))
                 [ exportName name, mapM_ exportTypeVar (hsq_explicit vars) ]

-- * Data definitions

exportDataKeyword :: Exporter (Located NewOrData)
exportDataKeyword (L l GHC.NewType) = export Schema.NewType l []
exportDataKeyword (L l GHC.DataType) = export Schema.DataType l []

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
  = export InfixConstructor l [ mapM_ exportTypeVar (maybe [] hsq_explicit tyVars), maybe (return ()) exportContext ctx
                              , exportType t1, defining (exportName name), exportType t2 ]

exportGADTConstructor :: HsName n => Exporter (Located (ConDecl n))
exportGADTConstructor (L l (ConDeclGADT { con_names = names, con_type = hsib_body -> typ }))
  = do let (vars, ctx, t) = getTypeVarsAndCtx typ
       export GADTConstructor l [ defining (mapM_ exportName names)
                                , mapM_ exportTypeVar vars
                                , exportContext ctx
                                , exportGADTConType t ]
  where getTypeVarsAndCtx :: LHsType n -> ([LHsTyVarBndr n], LHsContext n, LHsType n)
        getTypeVarsAndCtx (L _ (HsForAllTy [] typ)) = getTypeVarsAndCtx typ
        getTypeVarsAndCtx (L _ (HsForAllTy bndrs typ)) = let (_,ctx,t) = getTypeVarsAndCtx typ in (bndrs, ctx, t)
        getTypeVarsAndCtx (L _ (HsQualTy ctx typ)) = let (vars,_,t) = getTypeVarsAndCtx typ in (vars, ctx, t)
        getTypeVarsAndCtx t = ([], L noSrcSpan [], t)

        exportGADTConType :: HsName n => Exporter (Located (HsType n))
        exportGADTConType (L l (HsFunTy (L rl (HsRecTy flds)) resType))
          = export GADTRecordType l [ mapM_ exportFieldDecl flds, exportType resType ]
        exportGADTConType typ = export GADTNormalType (getLoc typ) [ exportType typ ]


exportFieldDecl :: HsName n => Exporter (Located (ConDeclField n))
exportFieldDecl (L l (ConDeclField names typ _))
  = export FieldDecl l [ defining (mapM_ exportFieldOccName names), exportType typ ]

exportDerivings :: HsName n => Exporter (LHsDerivingClause n)
exportDerivings (L l (HsDerivingClause strat derivs))
  = export Derivings l [ exportDerivingStrategy strat
                       , mapM_ exportInstanceHead (map hsib_body (unLoc derivs)) ]

exportDerivingStrategy :: Exporter (Maybe (Located DerivStrategy))
exportDerivingStrategy Nothing = return ()
exportDerivingStrategy (Just (L l StockStrategy)) = export StockDerivingStrat l []
exportDerivingStrategy (Just (L l AnyclassStrategy)) = export AnyClassDerivingStrat l []
exportDerivingStrategy (Just (L l NewtypeStrategy)) = export NewtypeDerivingStrat l []

-- * Classes

exportClassElemSig :: HsName n => Exporter (Located (Sig n))
exportClassElemSig (L l (TypeSig names typ))
  = export ClassTypeSignature l [ mapM_ exportName names, exportType (hsib_body $ hswc_body typ) ]
exportClassElemSig (L l (FixSig fixity))
  = export ClassFixitySignature l [ exportFixitySignature (L l fixity) ]
exportClassElemSig (L l (ClassOpSig True [name] typ))
  = export ClassDefaultSignature l [ exportName name, exportType (hsib_body typ) ]
exportClassElemSig (L l (ClassOpSig False names typ))
  = export ClassTypeSignature l [ mapM_ exportName names, exportType (hsib_body typ) ]
exportClassElemSig (L l (MinimalSig _ formula))
  = export ClassMinimalSignature l [ exportMinimalFormula formula ]
exportClassElemSig (L l (InlineSig name prag))
  = export ClassInlinePragma l [ exportName name, exportInlinePragma (L l prag) ]
exportClassElemSig (L l s) = exportError "class element" s

exportMinimalFormula :: HsName n => Exporter (Located (BooleanFormula (Located n)))
exportMinimalFormula (L l (Var name)) = export MinimalName l [ exportName name ]
exportMinimalFormula (L l (Parens formula)) = export MinimalParen l [ exportMinimalFormula formula ]
exportMinimalFormula (L l (And formulas))
  = export MinimalAnd l [ mapM_ exportMinimalFormula formulas ]
exportMinimalFormula (L l (Or formulas))
  = export MinimalOr l [ mapM_ exportMinimalFormula formulas ]


-- * Instances

exportClassInstSigs :: HsName n => Exporter (Located (Sig n))
exportClassInstSigs (L l (TypeSig names typ))
  = export InstanceTypeSig l [ mapM_ exportName names, exportType (hsib_body $ hswc_body typ) ]
exportClassInstSigs (L l (ClassOpSig _ names typ))
  = export InstanceTypeSig l [ mapM_ exportName names, exportType (hsib_body typ) ]
exportClassInstSigs (L l (SpecInstSig _ typ))
  = export InstanceSpecializeInstance l [ exportType (hsib_body typ) ]
exportClassInstSigs (L l (SpecSig name (map hsib_body -> tys) (inl_act -> phase)))
  = export InstanceSpecialize l [ exportPhase (L l phase), exportName name, mapM_ exportType tys ]
exportClassInstSigs (L l (InlineSig name prag))
  = export InstanceInline l [ exportInlinePragma (L l prag) ]

exportInstanceTypeFamily :: HsName n => Exporter (Located (TyFamInstDecl n))
exportInstanceTypeFamily (L l (TyFamInstDecl eqn _)) = export InstanceTypeFamily l [ exportTypeEquation eqn ]

exportInstanceDataFamily :: HsName n => Exporter (Located (DataFamInstDecl n))
exportInstanceDataFamily (L l (DataFamInstDecl tc (hsib_body -> pats) _ (HsDataDefn dn ctx _ ks cons derivs) _))
  | all ((\case ConDeclH98{} -> True; _ -> False) . unLoc) cons
  = export InstanceDataDecl l [ exportDataKeyword (L l dn)
                              , exportContext ctx
                              , exportTypeNameAndPats tc pats
                              , mapM_ exportConstructor cons
                              , mapM_ exportDerivings (unLoc derivs)
                              ]
  | otherwise
  = export InstanceGDataDecl l [ exportDataKeyword (L l dn)
                               , exportContext ctx
                               , exportTypeNameAndPats tc pats
                               , exportKindSignature ks
                               , mapM_ exportGADTConstructor cons
                               , mapM_ exportDerivings (unLoc derivs)
                               ]
  where
    exportTypeNameAndPats tc pats
      | all (\p -> srcSpanEnd (getLoc tc) < srcSpanStart (getLoc p)) pats -- prefix instance head application
      = export ApplicationIH (combineSpans (getLoc tc : map getLoc pats))
          [ exportName tc, mapM_ exportType pats ]
    exportTypeNameAndPats tc (lhs:rhs:rest)
     = export InfixIH (combineLocated (lhs:rhs:rest))
         [ exportType lhs, exportOperator tc, exportType rhs, mapM_ exportType rest ]

-- * Pattern synonyms

exportPatternSynonymLhs :: HsName n => Located n -> Exporter (HsPatSynDetails (Located n))
exportPatternSynonymLhs n (PrefixPatSyn args)
  = export PrefixPatSynLhs (combineLocated (n:args))
      [ defining (exportName n), mapM_ exportName args ]
exportPatternSynonymLhs n (InfixPatSyn lhs rhs)
  = export InfixPatSynLhs (combineLocated [lhs,rhs])
      [ exportName lhs, defining (exportOperator n), exportName rhs ]
exportPatternSynonymLhs n (RecordPatSyn flds)
  = export RecordPatSynLhs (combineLocated (map recordPatSynSelectorId flds))
      [ defining (exportName n), mapM_ (exportName . recordPatSynSelectorId) flds ]

exportPatternSynonymRhs :: HsName n => HsPatSynDir n -> Exporter (Located (Pat n))
exportPatternSynonymRhs ImplicitBidirectional (L l pat)
  = export BidirectionalPatSyn l [ exportPattern (L l pat) ]
exportPatternSynonymRhs (ExplicitBidirectional mg) (L l pat)
  = export ExplicitBidirectionalPatSyn l [ exportPattern (L l pat)
                                         , mapM_ exportMatch (unLoc $ mg_alts mg) ]
exportPatternSynonymRhs Unidirectional (L l pat)
  = export UnidirectionalPatSyn l [ exportPattern (L l pat) ]

-- * Instances

exportInstanceRule :: HsName n => Exporter (Located (HsType n))
exportInstanceRule (L l ((HsForAllTy bndrs (unLoc -> HsQualTy ctx typ))))
  = export InstanceRule l [ mapM_ exportTypeVar bndrs
                          , exportContext ctx
                          , exportInstanceHead typ
                          ]
exportInstanceRule (L l (HsQualTy ctx typ))
  = export InstanceRule l [ return (), exportContext ctx, exportInstanceHead typ ]
exportInstanceRule typ@(L l _) = export InstanceRule l [ return (), return (), exportInstanceHead typ ]

exportInstanceHead :: HsName n => Exporter (Located (HsType n))
exportInstanceHead = exportInstanceHead' . cleanHsType

exportInstanceHead' (L l (HsForAllTy [] t)) = exportInstanceHead t
exportInstanceHead' (L l (HsTyVar _ tv)) = export ConstructorIH l [ exportName tv ]
exportInstanceHead' (L l (HsAppTy t1 t2))
  = export ApplicationIH l [ exportInstanceHead t1, exportType t2 ]
exportInstanceHead' (L l (HsParTy typ)) = export ParenIH l [ exportInstanceHead typ ]
exportInstanceHead' (L l (HsOpTy t1 op t2))
  = export InfixIH l [ exportType t1, exportOperator op, exportType t2 ]
exportInstanceHead' (L l t) = exportError "instance head" t


exportTypeEquation :: HsName n => Exporter (Located (TyFamInstEqn n))
exportTypeEquation (L l (TyFamEqn name (hsib_body -> [lhs, rhs]) _ res))
  | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs)
  = export InfixTypeEquation l [ exportType lhs, exportOperator name, exportType rhs, exportType res ]
exportTypeEquation (L l (TyFamEqn name types _ res))
  = export PrefixTypeEquation l [ exportName name, mapM_ exportType (hsib_body types), exportType res ]

exportFunDep :: HsName n => Exporter (Located (FunDep (Located n)))
exportFunDep (L l (lhs, rhs))
  = export FunctionalDependency l [ mapM_ exportName lhs, mapM_ exportName rhs ]

exportTypeFamily :: HsName n => Exporter (Located (FamilyDecl n))
exportTypeFamily (L l (FamilyDecl GHC.DataFamily name tyVars _ sig _))
  = export Schema.DataFamily l [ exportDeclHead name tyVars, exportFamilySig sig ]
exportTypeFamily (L l (FamilyDecl OpenTypeFamily name tyVars _ sig injectivity))
  = export Schema.TypeFamily l [ exportDeclHead name tyVars, exportFamilyResSig injectivity sig ]
exportTypeFamily (L l tf@(FamilyDecl (ClosedTypeFamily {}) _ _ _ _ _))
  = exportError "open type family" tf

exportFamilySig :: HsName n => Exporter (Located (FamilyResultSig n))
exportFamilySig (L l NoSig) = return ()
exportFamilySig (L l (KindSig k)) = exportKindSignature (Just k)
exportFamilySig (L l (TyVarSig _)) = error "exportFamilySig: TyVarSig in type family result signature"

exportFamilyResSig :: HsName n => Maybe (LInjectivityAnn n) -> Exporter (Located (FamilyResultSig n))
exportFamilyResSig Nothing (L l NoSig) = return ()
exportFamilyResSig Nothing (L l (KindSig k)) = export TFKindSig l [ exportKindSignature (Just k) ]
exportFamilyResSig Nothing (L l (TyVarSig tv)) = export TFTyVarSig l [ exportTypeVar tv ]
exportFamilyResSig (Just (L l (InjectivityAnn n deps))) _
  = export TFInjectivitySig l [ exportName n, mapM_ exportName deps ]

exportTypeFamilyDefault :: HsName n => Exporter (Located (TyFamDefltEqn n))
exportTypeFamilyDefault (L l (TyFamEqn con pats _ rhs))
  = export ClassTypeFamilyDefault l [ exportDeclHead con pats, exportType rhs ]

-- * Basics

exportCallConvention :: Exporter (Located CCallConv)
exportCallConvention (L l CCallConv) = export CCC l []
exportCallConvention (L l CApiConv) = export CApiCC l []
exportCallConvention (L l StdCallConv) = export StdCC l []
exportCallConvention (L l JavaScriptCallConv) = export JavaScriptCC l []
exportCallConvention (L l PrimCallConv) = export PrimCC l []

exportSafety :: Exporter (Located GHC.Safety)
exportSafety (L l _) | not (isGoodSrcSpan l) = return ()
exportSafety (L l PlaySafe) = export Safe l []
exportSafety (L l PlayInterruptible) = export Interruptible l []
exportSafety (L l PlayRisky) = export Unsafe l []

exportOverlap :: Exporter (Located OverlapMode)
exportOverlap (L l (NoOverlap _)) = export DisableOverlap l []
exportOverlap (L l (GHC.Overlappable _)) = export Schema.Overlappable l []
exportOverlap (L l (GHC.Overlapping _)) = export Schema.Overlapping l []
exportOverlap (L l (GHC.Overlaps _)) = export Schema.Overlaps l []
exportOverlap (L l (Incoherent _)) = export IncoherentOverlap l []

exportRole :: Exporter (Located (Maybe GHC.Role))
exportRole (L l (Just Nominal)) = export NominalRole l []
exportRole (L l (Just Representational)) = export RepresentationalRole l []
exportRole (L l (Just Phantom)) = export PhantomRole l []
exportRole (L l Nothing) = return ()

exportRewriteRule :: HsName n => Exporter (Located (RuleDecl n))
exportRewriteRule (L l (HsRule (L nameLoc (_, ruleName)) phase bndrs left _ right _))
  = export RewriteRule l [ exportPhase (L l phase)
                         , mapM_ exportRuleVar bndrs
                         , exportExpression left
                         , exportExpression right
                         , writeStringAttribute (unpackFS ruleName)
                         ]

exportRuleVar :: HsName n => Exporter (Located (RuleBndr n))
exportRuleVar (L l (RuleBndr n)) = export RuleVar l [ exportName n ]
exportRuleVar (L l (RuleBndrSig n k)) = export RuleSigVar l [ exportName n, exportType (hsib_body $ hswc_body k) ]

