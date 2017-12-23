-- {-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Patterns where

import {-# SOURCE #-} Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Literals
import Language.Haskell.Heed.Export.Templates
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Utilities

import Data.Data
import HsPat
import BasicTypes as GHC (Boxity(..))
import SrcLoc
import HsTypes
import HsExpr
import HsLit

-- data Pattern = Variable
--              | Wildcard
--              | Lazy
--              | As
--              | As
--   deriving Data

exportPattern :: HsName n => Located (Pat n) -> TrfType ()
exportPattern (L l (VarPat name))
  = export "Pattern" "Variable" l [ "pattern_name" .-> defining (exportName name) ]
exportPattern (L l (WildPat _)) = export "Pattern" "Wildcard" l []
exportPattern (L l (LazyPat pat))
  = export "Pattern" "Lazy" l [ "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (AsPat name pat))
  = export "Pattern" "As" l [ "pattern_name" .-> defining (exportName name)
                            , "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (ParPat pat))
  = export "Pattern" "Paren" l [ "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (BangPat pat))
  = export "Pattern" "Bang" l [ "inner_pattern" .-> exportPattern pat ]
exportPattern (L l (ListPat pats _ _))
  = export "Pattern" "List" l [ "element_patterns" .-> mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Boxed _))
  = export "Pattern" "Tuple" l [ "element_patterns" .-> mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Unboxed _))
  = export "Pattern" "UnboxedTuple" l [ "element_patterns" .-> mapM_ exportPattern pats ]
exportPattern (L l (PArrPat pats _))
  = export "Pattern" "ParallelArray" l [ "element_patterns" .-> mapM_ exportPattern pats ]
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
exportPattern (L l p@(ConPatOut {})) = exportError p -- compiler-generated patterns
exportPattern (L l p@(SigPatOut {})) = exportError p -- compiler-generated patterns
exportPattern (L l (ViewPat expr pat _))
  = export "Pattern" "View" l [ "expr" .-> exportExpression expr
                              , "pattern" .-> exportPattern pat ]
exportPattern (L l (SplicePat qq@(HsQuasiQuote {})))
  = export "Pattern" "QuasiQuotation" l [ "quote" .-> exportQuasiQuotation (L l qq) ]
exportPattern (L l (SplicePat splice))
  = export "Pattern" "Splice" l [ "splice" .-> exportSplice (L l splice) ]
exportPattern (L l (LitPat lit))
  = export "Pattern" "Literal" l [ "literal" .-> exportMonoLiteral (L l lit) ]
exportPattern (L l (SigPatIn pat (hsib_body . hswc_body -> typ)))
  = export "Pattern" "Typed" l [ "pattern" .-> exportPattern pat
                               , "type" .-> exportType typ ]
exportPattern (L l (NPat (fmap ol_val -> lit) _ _ _))
  = export "Pattern" "Literal" l [ "literal" .-> exportPolyLiteral lit ]
exportPattern (L l (NPlusKPat id (fmap ol_val -> lit) _ _ _ _))
  = export "Pattern" "Literal" l [ "name" .-> exportName id
                                 , "literal" .-> exportPolyLiteral lit ]
exportPattern (L l (CoPat _ pat _)) = exportPattern (L l pat) -- coercion pattern introduced by GHC
exportPattern (L l (SumPat pat _ _ _))
  = export "Pattern" "Sum" l [ "actual_pattern" .-> exportPattern pat ]

exportPatternField :: HsName n => Located (HsRecField n (LPat n)) -> TrfType ()
exportPatternField (L l (HsRecField id arg False))
  = export "PatternField" "Prefix" l [ "field_name" .-> exportFieldOccName id
                                     , "field_pattern" .-> exportPattern arg ]
exportPatternField (L l (HsRecField id _ True))
  = export "PatternField" "Pun" l [ "field_name" .-> exportFieldOccName id ]

