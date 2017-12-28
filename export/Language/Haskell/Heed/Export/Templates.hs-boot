module Language.Haskell.Heed.Export.Templates where
import Language.Haskell.Heed.Export.Utilities

import HsExpr
import SrcLoc

exportSplice :: HsName n => Exporter (Located (HsSplice n))
exportQuasiQuotation :: HsName n => Exporter (Located (HsSplice n))
exportBracket :: HsName n => Exporter (Located (HsBracket n))
