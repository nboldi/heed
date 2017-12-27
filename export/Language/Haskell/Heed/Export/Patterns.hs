-- {-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Patterns (exportPattern, exportPatternField) where

import {-# SOURCE #-} Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Literals
import Language.Haskell.Heed.Export.Templates
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data (Data(..))
import HsPat
import BasicTypes as GHC (Boxity(..))
import SrcLoc
import HsTypes
import HsExpr
import HsLit

exportPattern :: HsName n => Exporter (Located (Pat n))
exportPattern (L l (VarPat name)) = export VariableP l [ defining (exportName name) ]
exportPattern (L l (WildPat _)) = export WildcardP l []
exportPattern (L l (LazyPat pat)) = export LazyP l [ exportPattern pat ]
exportPattern (L l (AsPat name pat))
  = export AsP l [ defining (exportName name), exportPattern pat ]
exportPattern (L l (ParPat pat)) = export ParenP l [ exportPattern pat ]
exportPattern (L l (BangPat pat)) = export BangP l [ exportPattern pat ]
exportPattern (L l (ListPat pats _ _)) = export ListP l [ mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Boxed _)) = export TupleP l [ mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Unboxed _)) = export UnboxedTupleP l [ mapM_ exportPattern pats ]
exportPattern (L l (PArrPat pats _)) = export ParallelArrayP l [ mapM_ exportPattern pats ]
exportPattern (L l (ConPatIn name (PrefixCon args)))
  = export PrefixConstructorP l [ exportName name, mapM_ exportPattern args ]
exportPattern (L l (ConPatIn name (RecCon (HsRecFields flds _))))
  = export RecordConstructorP l [ exportName name, mapM_ exportPatternField flds ]
exportPattern (L l (ConPatIn name (InfixCon left right)))
  = export RecordConstructorP l [ exportPattern left, exportOperator name, exportPattern right ]
exportPattern (L l p@(ConPatOut {})) = exportError "pattern" p -- compiler-generated patterns
exportPattern (L l p@(SigPatOut {})) = exportError "pattern" p -- compiler-generated patterns
exportPattern (L l (ViewPat expr pat _))
  = export ViewP l [ exportExpression expr, exportPattern pat ]
exportPattern (L l (SplicePat qq@(HsQuasiQuote {})))
  = export QuasiQuotationP l [ exportQuasiQuotation (L l qq) ]
exportPattern (L l (SplicePat splice))
  = export SpliceP l [ exportSplice (L l splice) ]
exportPattern (L l (LitPat lit))
  = export LiteralP l [ exportMonoLiteral (L l lit) ]
exportPattern (L l (SigPatIn pat (hsib_body . hswc_body -> typ)))
  = export TypedP l [ exportPattern pat, exportType typ ]
exportPattern (L l (NPat (fmap ol_val -> lit) _ _ _))
  = export LiteralP l [ exportPolyLiteral lit ]
exportPattern (L l (NPlusKPat id (fmap ol_val -> lit) _ _ _ _))
  = export LiteralP l [ exportName id, exportPolyLiteral lit ]
exportPattern (L l (CoPat _ pat _)) = exportPattern (L l pat) -- coercion pattern introduced by GHC
exportPattern (L l (SumPat pat _ _ _))
  = export SumP l [ exportPattern pat ]

exportPatternField :: HsName n => Exporter (Located (HsRecField n (LPat n)))
exportPatternField (L l (HsRecField id arg False))
  = export Prefix l [ exportFieldOccName id, exportPattern arg ]
exportPatternField (L l (HsRecField id _ True))
  = export Pun l [ exportFieldOccName id ]

