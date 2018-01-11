{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
module Language.Haskell.Heed.Export.Templates where

import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Declarations
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Schema

import Control.Monad.IO.Class
import Control.Monad.Writer
import HsExpr
import FastString
import SrcLoc

exportSplice :: HsName n => Exporter (Located (HsSplice n))
exportSplice spl = do exportSplice' spl
                      tell (ExportStore [] [getLoc spl])
                      maybe (return ()) (forceNameExport . exportSplice') =<< (renameSplice spl)

exportSplice' :: HsName n => Exporter (Located (HsSplice n))
exportSplice' (L l (HsTypedSplice _ _ expr)) = export Splice l [ exportExpression expr ]
exportSplice' (L l (HsUntypedSplice _ _ expr)) = export Splice l [ exportExpression expr ]
exportSplice' (L l sp@(HsSpliced _ (HsSplicedExpr expr))) = export Splice l [ exportExpression (L l expr) ]
exportSplice' (L l sp@(HsSpliced _ (HsSplicedTy t))) = export Splice l [ exportType (L l t) ]
exportSplice' (L l sp@(HsSpliced _ (HsSplicedPat p))) = export Splice l [ exportPattern (L l p) ]
exportSplice' (L l sp@(HsQuasiQuote{})) = exportError "splice" sp -- should be exported with 'exportQuasiQuotation'

exportQuasiQuotation :: HsName n => Exporter (Located (HsSplice n))
exportQuasiQuotation qq@(L l (HsQuasiQuote _ id l' str))
  = do export QuasiQuotation l [ exportName (L l' id), writeStringAttribute (unpackFS str) ]
       tell (ExportStore [] [l])
       (maybe (return ()) (forceNameExport . exportQuasiQuotation) =<< (renameSplice qq))
exportQuasiQuotation (L l qq) = exportError "quasi quotation" qq

exportBracket :: HsName n => Exporter (Located (HsBracket n))
exportBracket br = do exportBracket' br
                      tell (ExportStore [] [getLoc br])
                      maybe (return ()) (forceNameExport . exportBracket') =<< (renameBracket br)

exportBracket' :: HsName n => Exporter (Located (HsBracket n))
exportBracket' (L l (ExpBr expr)) = export ExprBracket l [ exportExpression expr ]
exportBracket' (L l (TExpBr expr)) = export ExprBracket l [ exportExpression expr ]
exportBracket' (L l (VarBr isSingle n))
  = export ExprBracket l [ export VarE nameLoc [ exportName (L nameLoc n) ] ]
  where nameLoc = updateStart (updateCol (if isSingle then (+1) else (+2))) l
exportBracket' (L l (PatBr pat)) = export PatternBracket l [ exportPattern pat ]
exportBracket' (L l (DecBrL decls)) = export DeclarationBracket l [ mapM_ exportDeclaration decls ]
exportBracket' (L l (DecBrG decls)) = export DeclarationBracket l [ exportDeclarationGroup decls ]
exportBracket' (L l (TypBr typ)) = export PatternBracket l [ exportType typ ]


