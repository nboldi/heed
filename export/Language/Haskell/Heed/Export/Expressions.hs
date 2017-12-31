{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Expressions (exportExpression) where

import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Literals
import Language.Haskell.Heed.Export.Types
import {-# SOURCE #-} Language.Haskell.Heed.Export.Templates
import Language.Haskell.Heed.Export.Statements
import {-# SOURCE #-} Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema hiding (Match(..))

import HsExpr
import HsPat
import HsBinds
import HsLit
import HsTypes
import BasicTypes (Boxity(..))
import OccName
import SrcLoc

exportExpression :: HsName n => Exporter (Located (HsExpr n))
exportExpression (L l (OpApp e1 (L _ (HsVar op)) _ e2))
  = export InfixAppE l [ exportExpression e1, exportOperator op, exportExpression e2 ]
exportExpression (L l (HsVar name)) = export VarE l [ exportName name ]
exportExpression (L l (HsUnboundVar name))
  = export VarE l [ writeStringAttribute (occNameString (unboundVarOcc name)) ]
exportExpression (L l (HsRecFld fld)) = export VarE l [ exportAmbiguousFieldName (L l fld) ]
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
exportExpression (L _ opApp@(OpApp _ (L _ op) _ _)) = exportError "operator expression" opApp
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
      $ export LambdaCaseE l [ exportExpression expr, mapM_ exportMatch cases ]
exportExpression (L l (HsIf _ expr thenE elseE))
  = export IfE l [ exportExpression expr, exportExpression thenE, exportExpression elseE ]
exportExpression (L l (HsMultiIf _ parts))
  = export MultiIfE l [ mapM_ exportRhss parts ]
exportExpression (L l (HsLet (unLoc -> binds) expr))
  = addToScope l $ export LetE l [ exportLocalBinds (L l binds), exportExpression expr ]
exportExpression (L l (HsDo DoExpr (unLoc -> stmts) _))
  = export DoE l [ scopedSequence (exportDoStatement exportExpression) stmts ]
exportExpression (L l (HsDo MDoExpr (unLoc -> [unLoc -> RecStmt { recS_stmts = stmts }, lastStmt]) _))
  = export MDoE l [ scopedSequence (exportDoStatement exportExpression) (stmts ++ [lastStmt]) ]
exportExpression (L l (HsDo MDoExpr (unLoc -> stmts) _))
  = export MDoE l [ scopedSequence (exportDoStatement exportExpression) stmts ]
exportExpression (L l (HsDo ListComp (unLoc -> stmts) _))
  = export ListCompE l [ exportListCompStatements stmts ]
exportExpression (L l (HsDo MonadComp (unLoc -> stmts) _))
  = export ListCompE l [ exportListCompStatements stmts ]
exportExpression (L l (HsDo PArrComp (unLoc -> stmts) _))
  = export ParallelArrayCompE l [ exportListCompStatements stmts ]
exportExpression (L l (ExplicitList _ _ exprs)) = export ListE l [ mapM_ exportExpression exprs ]
exportExpression (L l (ExplicitPArr _ exprs))
  = export ParallelArrayE l [ mapM_ exportExpression exprs ]
exportExpression (L l (RecordCon name _ _ fields))
  = export RecordConstructE l [ exportName name, exportFieldInits (L l fields) ]
exportExpression (L l (RecordUpd expr fields _ _ _ _))
  = export RecordUpdateE l [ exportExpression expr, mapM_ exportFieldUpdate fields ]
exportExpression (L l (ExprWithTySig expr typ))
  = export TypedE l [ exportExpression expr, exportType (hsib_body $ hswc_body typ) ]

exportExpression (L l (ArithSeq _ _ (From from)))
  = export EnumE l [ exportExpression from ]
exportExpression (L l (ArithSeq _ _ (FromThen from step)))
  = export EnumE l [ exportExpression from, exportExpression step ]
exportExpression (L l (ArithSeq _ _ (FromTo from to)))
  = export EnumE l [ exportExpression from, return (), exportExpression to ]
exportExpression (L l (ArithSeq _ _ (FromThenTo from step to)))
  = export EnumE l [ exportExpression from, exportExpression step, exportExpression to ]

exportExpression (L l (PArrSeq _ (FromTo from to)))
  = export ParallelArrayEnumE l [ exportExpression from, return (), exportExpression to ]
exportExpression (L l (PArrSeq _ (FromThenTo from step to)))
  = export ParallelArrayEnumE l [ exportExpression from, exportExpression step, exportExpression to ]

exportExpression (L l (HsBracket brack)) = export BracketE l [ exportBracket (L l brack) ]
exportExpression (L l (HsRnBracketOut br _)) = export BracketE l [ exportBracket (L l br) ]
exportExpression (L l (HsSpliceE qq@(HsQuasiQuote {})))
  = export QuasiQouteE l [ exportQuasiQuotation (L l qq) ]
exportExpression (L l (HsSpliceE splice)) = export SpliceE l [ exportSplice (L l splice) ]

-- trfExpr' (HsProc pat cmdTop) = AST.UProc <$> trfPattern pat <*> trfCmdTop cmdTop

exportExpression (L l (HsStatic _ expr)) = export StaticE l [ exportExpression expr ]
exportExpression (L l (HsAppType expr typ))
  = export TypeApplicationE l [ exportExpression expr, exportType (hswc_body typ) ]



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
exportExpression (L l (HsWrap _ expr)) = exportExpression (L l expr)
exportExpression (L l expr) = exportError "expression" expr


exportFieldInits :: HsName n => Exporter (Located (HsRecFields n (LHsExpr n)))
-- TODO: implicit field info
-- (createImplicitFldInfo (unLoc . (\(HsVar n) -> n) . unLoc) (map unLoc implicitFlds))
exportFieldInits (L l (HsRecFields fields dotdot))
  = export FieldUpdates l [ mapM_ exportFieldInit fields, maybe (return ()) (\_ -> export FieldWildcard noSrcSpan []) dotdot ]

exportFieldInit :: HsName n => Exporter (Located (HsRecField n (LHsExpr n)))
exportFieldInit (L l (HsRecField lbl _ True)) = export FieldPun l [ exportFieldOccName lbl ]
exportFieldInit (L l (HsRecField lbl arg False))
  = export NormalFieldUpdate l [ exportFieldOccName lbl, exportExpression arg ]

exportFieldUpdate :: HsName n => Exporter (Located (HsRecField' (AmbiguousFieldOcc n) (LHsExpr n)))
exportFieldUpdate (L l (HsRecField lbl _ True)) = export FieldPun l [ exportAmbiguousFieldName lbl ]
exportFieldUpdate (L l (HsRecField lbl val False))
  = export NormalFieldUpdate l [ exportAmbiguousFieldName lbl, exportExpression val ]
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
