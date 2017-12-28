{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
module Language.Haskell.Heed.Export.Templates where

import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Declarations
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import HsExpr
import FastString
import SrcLoc

exportSplice :: HsName n => Exporter (Located (HsSplice n))
exportSplice (L l (HsTypedSplice _ _ expr)) = export Splice l [ exportExpression expr ]
exportSplice (L l (HsUntypedSplice _ _ expr)) = export Splice l [ exportExpression expr ]
exportSplice (L l sp@(HsQuasiQuote{})) = exportError "splice" sp -- should be exported with 'exportQuasiQuotation'
exportSplice (L l sp@(HsSpliced{})) = exportError "splice" sp -- TODO: export this

exportQuasiQuotation :: HsName n => Exporter (Located (HsSplice n))
exportQuasiQuotation (L l (HsQuasiQuote _ id l' str))
  = export QuasiQuotation l [ exportName (L l' id), writeStringAttribute (unpackFS str) ]
exportQuasiQuotation (L l qq) = exportError "quasi quotation" qq

exportBracket :: HsName n => Exporter (Located (HsBracket n))
exportBracket (L l (ExpBr expr)) = export ExprBracket l [ exportExpression expr ]
exportBracket (L l (TExpBr expr)) = export ExprBracket l [ exportExpression expr ]
exportBracket (L l (VarBr isSingle n))
  = export ExprBracket l [ export VarE nameLoc [ exportName (L nameLoc n) ] ]
  where nameLoc = updateStart (updateCol (if isSingle then (+1) else (+2))) l
exportBracket (L l (PatBr pat)) = export PatternBracket l [ exportPattern pat ]
exportBracket (L l (DecBrL decls)) = export DeclarationBracket l [ mapM_ exportDeclaration decls ]
exportBracket (L l (DecBrG decls)) = export DeclarationBracket l [ exportDeclarationGroup decls ]
exportBracket (L l (TypBr typ)) = export PatternBracket l [ exportType typ ]


