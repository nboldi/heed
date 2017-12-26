{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Expressions (exportExpression) where

import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Literals
import {-# SOURCE #-} Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema hiding (Literal(..), Match(..))

import Data.Data (Data(..))
import HsExpr
import HsBinds
import HsLit
import HsTypes
import BasicTypes (Boxity(..))
import SrcLoc

exportExpression :: HsName n => Located (HsExpr n) -> TrfType ()
exportExpression (L l (OpApp e1 (L _ (HsVar op)) _ e2))
  = export InfixAppE l [ exportExpression e1, exportName op, exportExpression e2 ]
exportExpression (L l (HsVar name)) = export VarE l [ exportName name ]
-- exportExpression (L l (HsUnboundVar name)) = AST.UVar <$> trfNameText (occNameString $ unboundVarOcc name)
exportExpression (L l (HsRecFld fld)) = export VarE l [ exportAmbiguousFieldOccName (L l fld) ]
exportExpression (L l (HsIPVar ip)) = export VarE l [ exportImplicitName (L l ip) ]
exportExpression (L l (HsOverLit (ol_val -> val))) = export LiteralE l [ exportPolyLiteral (L l val) ]
exportExpression (L l (HsLit val)) = export LiteralE l [ exportMonoLiteral (L l val) ]
exportExpression (L l (HsLam (unLoc . mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] (unLoc -> EmptyLocalBinds))])))
  = addToScope (combineLocated pats)
      $ export LambdaE l [ mapM_ exportPattern pats, exportExpression expr ]
exportExpression (L l (HsLamCase (unLoc . mg_alts -> matches)))
  = export LambdaCaseE l [ mapM_ exportMatch matches ]
exportExpression (L l (HsApp e1 e2)) = export AppE l [ exportExpression e1, exportExpression e2 ]
exportExpression (L l (OpApp e1 (unLoc -> HsVar op) _ e2))
  = export InfixAppE l [ exportExpression e1, exportOperator op, exportExpression e2 ]
exportExpression (L l (OpApp e1 (L l' (HsRecFld op)) _ e2))
  = export InfixAppE l [ exportExpression e1, exportAmbiguousOperator (L l' op), exportExpression e2 ]
exportExpression (L _ opApp@(OpApp _ (L _ op) _ _)) = exportError opApp
exportExpression (L l (NegApp e _)) = export PrefixAppE l [ exportExpression e ] -- TODO: name for negative sign
exportExpression (L l (HsPar (unLoc -> SectionL expr (unLoc -> HsVar op))))
  = export LeftSectionE l [ exportExpression expr, exportOperator op ]
exportExpression (L l (HsPar (unLoc -> SectionL expr (L l' (HsRecFld op)))))
  = export LeftSectionE l [ exportExpression expr, exportAmbiguousOperator (L l' op) ]
exportExpression (L l (HsPar (unLoc -> SectionR (unLoc -> HsVar op) expr)))
  = export RightSectionE l [ exportOperator op, exportExpression expr ]
exportExpression (L l (HsPar (unLoc -> SectionR (L l' (HsRecFld op)) expr)))
  = export RightSectionE l [ exportAmbiguousOperator (L l' op), exportExpression expr ]
exportExpression (L l (HsPar expr)) = export ParenE l [ exportExpression expr ]
exportExpression (L l (ExplicitTuple tupArgs box))
  = export tupType l [mapM_ ((\case (Present e) -> exportExpression e
                                    Missing{} -> return ()) . unLoc) tupArgs]
  where tupType = case (box, all tupArgPresent tupArgs) of
                    (Boxed, True) -> TupleE
                    (Unboxed, True) -> UnboxedTupleE
                    (Boxed, False) -> TupleSectionE
                    (Unboxed, False) -> UnboxedTupleSectionE
exportExpression (L l (HsCase expr (unLoc . mg_alts -> cases)))
  = addToScope (combineLocated cases)
      $ export LambdaCaseE l [ exportExpression expr, mapM_ exportAlternative cases ]
exportExpression (L l (HsIf _ expr thenE elseE))
  = export IfE l [ exportExpression expr, exportExpression thenE, exportExpression elseE ]
exportExpression (L l (HsMultiIf _ parts))
  = export MultiIfE l [ mapM_ exportCaseRhss parts ]
-- exportExpression (L l (HsLet (unLoc -> binds) expr))
--   = addToScope (combineLocated binds)
--       $ export Let l [ exportLocalBinds binds, exportExpression expr ]
-- exportExpression (L l (HsDo DoExpr (unLoc -> stmts) _))
--   =


-- trfExpr' (HsDo DoExpr (unLoc -> stmts) _) = AST.UDo <$> annLocNoSema (tokenLoc AnnDo) (pure AST.UDoKeyword)
--                                                     <*> makeNonemptyIndentedList (trfScopedSequence trfDoStmt stmts)
-- trfExpr' (HsDo MDoExpr (unLoc -> [unLoc -> RecStmt { recS_stmts = stmts }, lastStmt]) _)
--   = AST.UDo <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.UMDoKeyword)
--             <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt (stmts ++ [lastStmt])))
-- trfExpr' (HsDo MDoExpr (unLoc -> stmts) _) = AST.UDo <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.UMDoKeyword)
--                                                      <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt stmts))
-- trfExpr' (HsDo ListComp (unLoc -> stmts) _)
--   = AST.UListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
-- trfExpr' (HsDo MonadComp (unLoc -> stmts) _)
--   = AST.UListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
-- trfExpr' (HsDo PArrComp (unLoc -> stmts) _)
--   = AST.UParArrayComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
-- trfExpr' (ExplicitList _ _ exprs) = AST.UList <$> trfAnnList' ", " trfExpr exprs
-- trfExpr' (ExplicitPArr _ exprs) = AST.UParArray <$> trfAnnList' ", " trfExpr exprs
-- trfExpr' (RecordCon name _ _ fields) = AST.URecCon <$> trfName name <*> trfFieldInits fields
-- trfExpr' (RecordUpd expr fields _ _ _ _) = AST.URecUpdate <$> trfExpr expr <*> trfAnnList ", " trfFieldUpdate fields
-- trfExpr' (ExprWithTySig expr typ) = AST.UTypeSig <$> trfExpr expr <*> trfType (hsib_body $ hswc_body typ)
-- trfExpr' (ArithSeq _ _ (From from)) = AST.UEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
--                                                                 <*> nothing "" "" (before AnnCloseS)
-- trfExpr' (ArithSeq _ _ (FromThen from step))
--   = AST.UEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> nothing "" "" (before AnnCloseS)
-- trfExpr' (ArithSeq _ _ (FromTo from to))
--   = AST.UEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
--                                <*> (makeJust <$> trfExpr to)
-- trfExpr' (ArithSeq _ _ (FromThenTo from step to))
--   = AST.UEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> (makeJust <$> trfExpr to)
-- trfExpr' (PArrSeq _ (FromTo from to))
--   = AST.UParArrayEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot) <*> trfExpr to
-- trfExpr' (PArrSeq _ (FromThenTo from step to))
--   = AST.UParArrayEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> trfExpr to
-- trfExpr' (HsBracket brack) = AST.UBracketExpr <$> annContNoSema (trfBracket' brack)
-- trfExpr' (HsSpliceE qq@(HsQuasiQuote {})) = AST.UQuasiQuoteExpr <$> annContNoSema (trfQuasiQuotation' qq)
-- trfExpr' (HsSpliceE splice) = AST.USplice <$> trfSplice splice
-- trfExpr' (HsRnBracketOut br _) = AST.UBracketExpr <$> annContNoSema (trfBracket' br)
-- trfExpr' (HsProc pat cmdTop) = AST.UProc <$> trfPattern pat <*> trfCmdTop cmdTop
-- trfExpr' (HsStatic _ expr) = AST.UStaticPtr <$> trfExpr expr
-- trfExpr' (HsAppType expr typ) = AST.UExplTypeApp <$> trfExpr expr <*> trfType (hswc_body typ)
-- trfExpr' (HsSCC _ lit expr) = AST.UExprPragma <$> pragma <*> trfExpr expr
--   where pragma = do pragLoc <- tokensLoc [AnnOpen, AnnClose]
--                     focusOn pragLoc $ annContNoSema (AST.USccPragma <$> annLocNoSema (mappend <$> tokenLoc AnnValStr <*> tokenLocBack AnnVal) (trfText' lit))
-- trfExpr' (HsCoreAnn _ lit expr) = AST.UExprPragma <$> pragma <*> trfExpr expr
--   where pragma = do pragLoc <- tokensLoc [AnnOpen, AnnClose]
--                     focusOn pragLoc $ annContNoSema (AST.UCorePragma <$> annLocNoSema (mappend <$> tokenLoc AnnValStr <*> tokenLocBack AnnVal) (trfText' lit))
-- trfExpr' (HsTickPragma _ source _ expr) = AST.UExprPragma <$> pragma <*> trfExpr expr
--   where pragma = do pragLoc <- tokensLoc [AnnOpen, AnnClose]
--                     focusOn pragLoc $ annContNoSema (AST.UGeneratedPragma <$> (trfSourceRange source))
-- trfExpr' (ExplicitSum tag arity expr _)
--   = do sepsBefore <- focusBeforeLoc (srcSpanStart (getLoc expr)) (eachTokenLoc (AnnOpen : replicate (tag - 1) AnnVbar))
--        sepsAfter <- focusAfterLoc (srcSpanEnd (getLoc expr)) (eachTokenLoc (replicate (arity - tag) AnnVbar))
--        let locsBefore = map srcSpanEnd $ init sepsBefore
--            locsAfter = map srcSpanEnd sepsAfter
--        AST.UUnboxedSum <$> makeList " | " (after AnnOpen) (mapM makePlaceholder locsBefore)
--                        <*> trfExpr expr
--                        <*> makeList " | " (before AnnClose) (mapM makePlaceholder locsAfter)
--   where makePlaceholder l = annLocNoSema (pure (srcLocSpan l)) (pure AST.UUnboxedSumPlaceHolder)
-- trfExpr' EWildPat = return AST.UHole
-- trfExpr' t = unhandledElement "expression" t
--
-- trfFieldInits :: TransformName n r => HsRecFields n (LHsExpr n) -> Trf (AnnListG AST.UFieldUpdate (Dom r) RangeStage)
-- trfFieldInits (HsRecFields fields dotdot)
--   = do cont <- asks contRange
--        let (normalFlds, implicitFlds) = partition ((cont /=) . getLoc) fields
--        makeList ", " (before AnnCloseC)
--          $ ((++) <$> mapM trfFieldInit normalFlds
--                   <*> (if isJust dotdot then (:[]) <$> annLocNoSema (tokenLoc AnnDotdot)
--                                                                     (AST.UFieldWildcard <$> (annCont (createImplicitFldInfo (unLoc . (\(HsVar n) -> n) . unLoc) (map unLoc implicitFlds)) (pure AST.FldWildcard)))
--                                         else pure []))
--
-- trfFieldInit :: TransformName n r => Located (HsRecField n (LHsExpr n)) -> Trf (Ann AST.UFieldUpdate (Dom r) RangeStage)
-- trfFieldInit = trfLocNoSema $ \case
--   HsRecField id _ True -> AST.UFieldPun <$> trfName (getFieldOccName id)
--   HsRecField id val False -> AST.UNormalFieldUpdate <$> trfName (getFieldOccName id) <*> trfExpr val
--
-- trfFieldUpdate :: TransformName n r => HsRecField' (AmbiguousFieldOcc n) (LHsExpr n) -> Trf (AST.UFieldUpdate (Dom r) RangeStage)
-- trfFieldUpdate (HsRecField id _ True) = AST.UFieldPun <$> trfAmbiguousFieldName id
-- trfFieldUpdate (HsRecField id val False) = AST.UNormalFieldUpdate <$> trfAmbiguousFieldName id <*> trfExpr val
--
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
--
-- trfCmdTop :: TransformName n r => Located (HsCmdTop n) -> Trf (Ann AST.UCmd (Dom r) RangeStage)
-- trfCmdTop (L _ (HsCmdTop cmd _ _ _)) = trfCmd cmd
--
-- trfCmd :: TransformName n r => Located (HsCmd n) -> Trf (Ann AST.UCmd (Dom r) RangeStage)
-- trfCmd = trfLocNoSema trfCmd'
--
-- trfCmd' :: TransformName n r => HsCmd n -> Trf (AST.UCmd (Dom r) RangeStage)
-- trfCmd' (HsCmdArrApp left right _ typ dir) = AST.UArrowAppCmd <$> trfExpr left <*> op <*> trfExpr right
--   where op = case (typ, dir) of (HsFirstOrderApp, False) -> annLocNoSema (tokenLoc Annrarrowtail) (pure AST.URightAppl)
--                                 (HsFirstOrderApp, True) -> annLocNoSema (tokenLoc Annlarrowtail) (pure AST.ULeftAppl)
--                                 (HsHigherOrderApp, False) -> annLocNoSema (tokenLoc AnnRarrowtail) (pure AST.URightHighApp)
--                                 (HsHigherOrderApp, True) -> annLocNoSema (tokenLoc AnnLarrowtail) (pure AST.ULeftHighApp)
--                                                                        -- FIXME: needs a before
-- trfCmd' (HsCmdArrForm expr _ _ cmds) = AST.UArrowFormCmd <$> trfExpr expr <*> makeList " " (before AnnClose) (mapM trfCmdTop cmds)
-- trfCmd' (HsCmdApp cmd expr) = AST.UAppCmd <$> trfCmd cmd <*> trfExpr expr
-- trfCmd' (HsCmdLam (MG (unLoc -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] body] _)]) _ _ _))
--   = AST.ULambdaCmd <$> (makeNonemptyList " " $ mapM trfPattern pats) <*> trfCmd body
-- trfCmd' (HsCmdPar cmd) = AST.UParenCmd <$> trfCmd cmd
-- trfCmd' (HsCmdCase expr (MG (unLoc -> alts) _ _ _))
--   = AST.UCaseCmd <$> trfExpr expr <*> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfAlt' trfCmd)) alts)
-- trfCmd' (HsCmdIf _ pred thenExpr elseExpr) = AST.UIfCmd <$> trfExpr pred <*> trfCmd thenExpr <*> trfCmd elseExpr
-- trfCmd' (HsCmdLet (unLoc -> binds) cmd) = addToScope binds (AST.ULetCmd <$> trfLocalBinds AnnLet binds <*> trfCmd cmd)
-- trfCmd' (HsCmdDo (unLoc -> stmts) _) = AST.UDoCmd <$> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfDoStmt' trfCmd)) stmts)
-- -- | TODO: implement
-- trfCmd' (HsCmdLam {}) = convertionProblem "trfCmd': cmd lambda not supported yet"
-- trfCmd' (HsCmdWrap {}) = convertionProblem "trfCmd': cmd wrap not supported yet"
--
-- trfText' :: StringLiteral -> Trf (AST.UStringNode (Dom r) RangeStage)
-- trfText' = pure . AST.UStringNode . unpackFS . sl_fs
--
-- trfSourceRange :: (StringLiteral, (Int, Int), (Int, Int)) -> Trf (Ann AST.USourceRange (Dom r) RangeStage)
-- trfSourceRange (fileName, (startRow, startCol), (endRow, endCol))
--   = do fnLoc <- tokenLoc AnnValStr
--        [srLoc, scLoc, erLoc, ecLoc] <- allTokenLoc AnnVal
--        annLocNoSema (pure (fnLoc `combineSrcSpans` ecLoc))
--          (AST.USourceRange <$> annLocNoSema (pure fnLoc) (trfText' fileName)
--                            <*> annLocNoSema (pure srLoc) (pure $ AST.Number $ fromIntegral startRow)
--                            <*> annLocNoSema (pure scLoc) (pure $ AST.Number $ fromIntegral startCol)
--                            <*> annLocNoSema (pure erLoc) (pure $ AST.Number $ fromIntegral endRow)
--                            <*> annLocNoSema (pure ecLoc) (pure $ AST.Number $ fromIntegral endCol))
