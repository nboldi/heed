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
import Language.Haskell.Heed.Schema hiding (Match(..))

import Data.List
import HsExpr
import HsPat
import HsBinds
import HsLit
import HsTypes
import BasicTypes (Boxity(..))
import OccName
import SrcLoc
import Control.Monad.IO.Class

exportExpression :: HsName n => Exporter (Located (HsExpr n))
exportExpression (L l (OpApp e1 (L _ (HsVar op)) _ e2))
  = export InfixAppE l [ exportExpression e1, exportOperator op, exportExpression e2 ]
exportExpression (L l (HsVar name)) = export VarE l [ exportName (L l (unLoc name)) ]
exportExpression (L l (HsUnboundVar name))
  = export VarE l [ writeStringAttribute (occNameString (unboundVarOcc name)) ]
exportExpression (L l (HsRecFld fld)) = export VarE l [ exportAmbiguous exportName (L l fld) ]
exportExpression (L l (HsIPVar ip)) = export VarE l [ exportImplicitName (L l ip) ]
exportExpression (L l (HsOverLit (ol_val -> val))) = export LiteralE l [ exportPolyLiteral (L l val) ]
exportExpression (L l (HsLit val)) = export LiteralE l [ exportMonoLiteral (L l val) ]
exportExpression (L l (HsLam (unLoc . mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] (unLoc -> EmptyLocalBinds))])))
  = do ind <- writeInsert LambdaE l
       newScope l (goInto ind 1 $ mapM_ exportPattern pats)
                  (goInto ind 2 $ exportExpression expr)
exportExpression (L l (HsLamCase (unLoc . mg_alts -> matches)))
  = export LambdaCaseE l [ mapM_ exportMatch matches ]
exportExpression (L l (HsApp e1 e2)) = export AppE l [ exportExpression e1, exportExpression e2 ]
exportExpression (L l (OpApp e1 (unLoc . cleanExpr -> HsVar op) _ e2))
  = export InfixAppE l [ exportExpression e1, exportOperator op, exportExpression e2 ]
exportExpression (L l (OpApp e1 (cleanExpr -> L l' (HsRecFld op)) _ e2))
  = export InfixAppE l [ exportExpression e1, exportAmbiguous exportOperator (L l' op), exportExpression e2 ]
exportExpression (L _ (OpApp _ (cleanExpr -> L _ (HsConLikeOut {})) _ _)) = return () -- compiler-generated
exportExpression (L _ (OpApp _ (L l op) _ _)) = exportError "operator expression" (unLoc $ cleanExpr (L l op))
exportExpression (L l (NegApp e _)) = export PrefixAppE l [ exportExpression e ] -- TODO: name for negative sign
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionL expr (unLoc . cleanExpr -> HsVar op))))
  = export LeftSectionE l [ exportExpression expr, exportOperator op ]
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionL expr (L l' (HsRecFld op)))))
  = export LeftSectionE l [ exportExpression expr, exportAmbiguous exportOperator (L l' op) ]
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionL expr (unLoc . cleanExpr -> HsConLikeOut {}))))
  = exportExpression expr
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionL expr right)))
  = exportError "right section expression" (unLoc (cleanExpr right))
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionR (unLoc . cleanExpr -> HsVar op) expr)))
  = export RightSectionE l [ exportOperator op, exportExpression expr ]
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionR (L l' (HsRecFld op)) expr)))
  = export RightSectionE l [ exportAmbiguous exportOperator (L l' op), exportExpression expr ]
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionR (unLoc . cleanExpr -> HsConLikeOut {}) expr)))
  = exportExpression expr
exportExpression (L l (HsPar (unLoc . cleanExpr -> SectionR left expr)))
  = exportError "left section expression" (unLoc (cleanExpr left))
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
  = export LambdaCaseE l [ exportExpression expr, mapM_ exportMatch cases ]
exportExpression (L l (HsIf _ expr thenE elseE))
  = export IfE l [ exportExpression expr, exportExpression thenE, exportExpression elseE ]
exportExpression (L l (HsMultiIf _ parts))
  = export MultiIfE l [ mapM_ exportRhss parts ]
exportExpression (L l (HsLet (unLoc -> binds) expr))
  = newScope_ l $ export LetE l [ exportLocalBinds (L l binds), exportExpression expr ]
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
exportExpression (L l (HsProc pat cmdTop)) = export ProcE l [ exportPattern pat, exportCmdTop cmdTop ]
exportExpression (L l (HsStatic _ expr)) = export StaticE l [ exportExpression expr ]
exportExpression (L l (HsAppType expr typ))
  = export TypeApplicationE l [ exportExpression expr, exportType (hswc_body typ) ]
exportExpression (L l (HsConLikeOut {})) = return () -- compiler generated thing
exportExpression (L l (HsAppTypeOut {})) = return () -- compiler generated thing
exportExpression (L l (HsTcBracketOut {})) = return () -- compiler generated thing
exportExpression (L l (ExprWithTySigOut {})) = return () -- compiler generated thing

-- TODO: export pragmas as well
exportExpression (L l (HsSCC _ _ expr)) = exportExpression expr
exportExpression (L l (HsCoreAnn _ _ expr)) = exportExpression expr
exportExpression (L l (HsTickPragma _ _ _ expr)) = exportExpression expr

exportExpression (L l (ExplicitSum _ _ expr _)) = export UnboxedSumE l [ exportExpression expr ]
exportExpression (L l EWildPat) = export WildPatE l []--
exportExpression (L l (HsWrap _ expr)) = exportExpression (L l expr)
exportExpression (L l expr) = exportError "expression" expr

cleanExpr :: LHsExpr n -> LHsExpr n
cleanExpr (L l (HsWrap _ expr)) = L l expr
cleanExpr expr = expr

exportFieldInits :: HsName n => Exporter (Located (HsRecFields n (LHsExpr n)))
exportFieldInits (L l (HsRecFields fields dotdot))
  = export FieldUpdates l [ mapM_ exportFieldInit normalFlds
                          , do maybe (return ()) (\_ -> export FieldWildcard noSrcSpan []) dotdot
                               writeImplicitInfo (hsGetNames . (\(HsVar n) -> n) . unLoc) (map unLoc implicitFlds)
                          ]
  where (normalFlds, implicitFlds) = partition ((l /=) . getLoc) fields

exportFieldInit :: HsName n => Exporter (Located (HsRecField n (LHsExpr n)))
exportFieldInit (L l (HsRecField lbl _ True)) = export FieldPun l [ exportFieldOccName lbl ]
exportFieldInit (L l (HsRecField lbl arg False))
  = export NormalFieldUpdate l [ exportFieldOccName lbl, exportExpression arg ]

exportFieldUpdate :: HsName n => Exporter (Located (HsRecField' (AmbiguousFieldOcc n) (LHsExpr n)))
exportFieldUpdate (L l (HsRecField lbl _ True)) = export FieldPun l [ exportAmbiguous exportName lbl ]
exportFieldUpdate (L l (HsRecField lbl val False))
  = export NormalFieldUpdate l [ exportAmbiguous exportName lbl, exportExpression val ]

-- * Export commands

exportCmdTop :: HsName n => Exporter (Located (HsCmdTop n))
exportCmdTop (L _ (HsCmdTop cmd _ _ _)) = exportCmd cmd

exportCmd :: HsName n => Exporter (Located (HsCmd n))
exportCmd (L l (HsCmdArrApp left right _ typ dir))
  = export ArrowAppCmd l [ exportExpression left, export arrow l [], exportExpression right ]
  where arrow = case (typ, dir) of (HsFirstOrderApp, False) -> RightApp
                                   (HsFirstOrderApp, True) -> LeftApp
                                   (HsHigherOrderApp, False) -> RightHighApp
                                   (HsHigherOrderApp, True) -> LeftHighApp
exportCmd (L l (HsCmdArrForm expr _ _ cmds))
  = export ArrowFormCmd l [ exportExpression expr, mapM_ exportCmdTop cmds ]
exportCmd (L l (HsCmdApp cmd expr)) = export AppCmd l [ exportCmd cmd, exportExpression expr ]
exportCmd (L l (HsCmdLam (MG (unLoc -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] body] _)]) _ _ _)))
  = newScope_ l $ export LambdaCmd l [ mapM_ exportPattern pats, exportCmd body ]
exportCmd (L l (HsCmdPar cmd)) = export ParenCmd l [ exportCmd cmd ]
exportCmd (L l (HsCmdCase expr (MG (unLoc -> alts) _ _ _)))
  = export CaseCmd l [ exportExpression expr, mapM_ (gExportMatch exportCmd) alts ]
exportCmd (L l (HsCmdIf _ pred thenExpr elseExpr))
  = export IfCmd l [ exportExpression pred, exportCmd thenExpr, exportCmd elseExpr ]
exportCmd (L l (HsCmdLet binds cmd))
  = newScope_ l $ export LetCmd l [ exportLocalBinds binds, exportCmd cmd ]
exportCmd (L l (HsCmdDo (unLoc -> stmts) _))
  = export DoCmd l [ scopedSequence (exportDoStatement exportCmd) stmts ]
exportCmd (L l cmd) = exportError "command" cmd
