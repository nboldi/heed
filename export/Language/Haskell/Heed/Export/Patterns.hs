-- TODO: force completeness with  {-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module Language.Haskell.Heed.Export.Patterns where

import {-# SOURCE #-} Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Utilities

import HsPat
import BasicTypes as GHC (Boxity(..))
import SrcLoc
import HsTypes

exportPattern :: HsName n => Located (Pat n) -> TrfType ()
exportPattern (L l (VarPat name)) = export "Pattern" "Variable" l [ "pattern_name" .-> defining (exportName name) ]
exportPattern (L l (WildPat _)) = export "Pattern" "Wildcard" l []
exportPattern (L l (LazyPat pat)) = export "Pattern" "Lazy" l [ "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (AsPat name pat)) = export "Pattern" "As" l [ "pattern_name" .-> defining (exportName name) 
                                                            , "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (ParPat pat)) = export "Pattern" "Paren" l [ "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (BangPat pat)) = export "Pattern" "Bang" l [ "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (ListPat pats _ _)) = export "Pattern" "List" l [ "element_patterns" .-> mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Boxed _)) = export "Pattern" "Tuple" l [ "element_patterns" .-> mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Unboxed _)) = export "Pattern" "UnboxedTuple" l [ "element_patterns" .-> mapM_ exportPattern pats ]
exportPattern (L l (PArrPat pats _)) = export "Pattern" "ParallelArray" l [ "element_patterns" .-> mapM_ exportPattern pats ]
exportPattern (L l (ConPatIn name (PrefixCon args))) 
  = export "Pattern" "PrefixConstructor" l [ "constructor_name" .-> exportName name
                                      , "argument_patterns" .-> mapM_ exportPattern args ]
exportPattern (L l (ConPatIn name (RecCon (HsRecFields flds _))))
  = export "Pattern" "RecordConstructor" l [ "constructor_name" .-> exportName name
                                           , "argument_patterns" .-> mapM_ exportPatternField flds ]
exportPattern (L l (ConPatIn name (InfixCon left right))) 
  = export "Pattern" "RecordConstructor" l [ "left_operand" .-> exportPattern left
                                           , "operator" .-> exportOperator name
                                           , "right_operand" .-> exportPattern right ]
exportPattern (L l (ViewPat expr pat _))
  = export "Pattern" "View" l [ "expr" .-> exportExpression expr
                              , "pattern" .-> exportPattern pat ]
-- trfPattern (L l (SplicePat qq@(HsQuasiQuote {}))) = AST.UQuasiQuotePat <$> annContNoSema (trfQuasiQuotation' qq)
-- trfPattern (L l (SplicePat splice)) = AST.USplicePat <$> trfSplice splice

-- trfPattern (L l (LitPat lit)) = AST.ULitPat <$> annCont (pure $ RealLiteralInfo (monoLiteralType lit)) (trfLiteral' lit)
-- trfPattern (L l (SigPatIn pat (hsib_body . hswc_body -> typ))) = AST.UTypeSigPat <$> trfPattern pat <*> trfType typ
-- trfPattern (L l (NPat (ol_val . unLoc -> lit) _ _ _)) = AST.ULitPat <$> annCont (asks contRange >>= pure . PreLiteralInfo) (trfOverloadedLit lit)
-- trfPattern (L l (NPlusKPat id (L l lit) _ _ _ _)) = AST.UNPlusKPat <$> define (trfName id) <*> annLoc (asks contRange >>= pure . PreLiteralInfo) (pure l) (trfOverloadedLit (ol_val lit))
-- trfPattern (L l (CoPat _ pat _)) = trfPattern' pat -- coercion pattern introduced by GHC
-- trfPattern (L l (SumPat pat tag arity _))
--   = do sepsBefore <- focusBeforeLoc (srcSpanStart (getLoc pat)) (eachTokenLoc (AnnOpen : replicate (tag - 1) AnnVbar))
--        sepsAfter <- focusAfterLoc (srcSpanEnd (getLoc pat)) (eachTokenLoc (replicate (arity - tag) AnnVbar))
--        let locsBefore = map srcSpanEnd $ init sepsBefore
--            locsAfter = map srcSpanEnd sepsAfter
--        AST.UUnboxedSumPat <$> makeList " | " (after AnnOpen) (mapM makePlaceholder locsBefore)
--                           <*> trfPattern pat
--                           <*> makeList " | " (before AnnClose) (mapM makePlaceholder locsAfter)
--   where makePlaceholder l = annLocNoSema (pure (srcLocSpan l)) (pure AST.UUnboxedSumPlaceHolder)

-- trfPattern p = unhandledElement "pattern" p

exportPatternField :: HsName n => Located (HsRecField n (LPat n)) -> TrfType ()
exportPatternField (L l (HsRecField id arg False))
  = export "PatternField" "Prefix" l [ "field_name" .-> exportFieldOccName id
                                     , "field_pattern" .-> exportPattern arg ]
exportPatternField' (L l (HsRecField id _ True))
  = export "PatternField" "Pun" l [ "field_name" .-> exportFieldOccName id ]

