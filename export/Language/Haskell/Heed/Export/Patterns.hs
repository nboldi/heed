-- {-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Patterns (exportPattern, exportPatternField) where

import {-# SOURCE #-} Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Literals
import Language.Haskell.Heed.Export.Templates
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Utilities hiding (Node(..))
import Language.Haskell.Heed.Export.Utilities (Node(Pattern, PatternField))

import Data.Data (Data(..))
import HsPat
import BasicTypes as GHC (Boxity(..))
import SrcLoc
import HsTypes
import HsExpr
import HsLit

data Pattern = Variable | Wildcard | Lazy | As | Paren | Bang | List | Tuple | UnboxedTuple
             | ParallelArray | PrefixConstructor | RecordConstructor | View | QuasiQuotation
             | Splice | Typed | Sum | Literal
  deriving Data

data PatternField = Prefix | Pun deriving Data

exportPat = export Pattern

exportPattern :: HsName n => Located (Pat n) -> TrfType ()
exportPattern (L l (VarPat name))
  = exportPat Variable l [ defining (exportName name) ]
exportPattern (L l (WildPat _)) = exportPat Wildcard l []
exportPattern (L l (LazyPat pat))
  = exportPat Lazy l [ exportPattern pat ]
exportPattern (L l (AsPat name pat))
  = exportPat As l [ defining (exportName name), exportPattern pat ]
exportPattern (L l (ParPat pat))
  = exportPat Paren l [ exportPattern pat ]
exportPattern (L l (BangPat pat))
  = exportPat Bang l [ exportPattern pat ]
exportPattern (L l (ListPat pats _ _))
  = exportPat List l [ mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Boxed _))
  = exportPat Tuple l [ mapM_ exportPattern pats ]
exportPattern (L l (TuplePat pats Unboxed _))
  = exportPat UnboxedTuple l [ mapM_ exportPattern pats ]
exportPattern (L l (PArrPat pats _))
  = exportPat ParallelArray l [ mapM_ exportPattern pats ]
exportPattern (L l (ConPatIn name (PrefixCon args)))
  = exportPat PrefixConstructor l [ exportName name, mapM_ exportPattern args ]
exportPattern (L l (ConPatIn name (RecCon (HsRecFields flds _))))
  = exportPat RecordConstructor l [ exportName name, mapM_ exportPatternField flds ]
exportPattern (L l (ConPatIn name (InfixCon left right)))
  = exportPat RecordConstructor l [ exportPattern left, exportOperator name, exportPattern right ]
exportPattern (L l p@(ConPatOut {})) = exportError p -- compiler-generated patterns
exportPattern (L l p@(SigPatOut {})) = exportError p -- compiler-generated patterns
exportPattern (L l (ViewPat expr pat _))
  = exportPat View l [ exportExpression expr, exportPattern pat ]
exportPattern (L l (SplicePat qq@(HsQuasiQuote {})))
  = exportPat QuasiQuotation l [ exportQuasiQuotation (L l qq) ]
exportPattern (L l (SplicePat splice))
  = exportPat Splice l [ exportSplice (L l splice) ]
exportPattern (L l (LitPat lit))
  = exportPat Literal l [ exportMonoLiteral (L l lit) ]
exportPattern (L l (SigPatIn pat (hsib_body . hswc_body -> typ)))
  = exportPat Typed l [ exportPattern pat, exportType typ ]
exportPattern (L l (NPat (fmap ol_val -> lit) _ _ _))
  = exportPat Literal l [ exportPolyLiteral lit ]
exportPattern (L l (NPlusKPat id (fmap ol_val -> lit) _ _ _ _))
  = exportPat Literal l [ exportName id, exportPolyLiteral lit ]
exportPattern (L l (CoPat _ pat _)) = exportPattern (L l pat) -- coercion pattern introduced by GHC
exportPattern (L l (SumPat pat _ _ _))
  = exportPat Sum l [ exportPattern pat ]

exportPatternField :: HsName n => Located (HsRecField n (LPat n)) -> TrfType ()
exportPatternField (L l (HsRecField id arg False))
  = export PatternField Prefix l [ exportFieldOccName id
                                     , exportPattern arg ]
exportPatternField (L l (HsRecField id _ True))
  = export PatternField Pun l [ exportFieldOccName id ]

