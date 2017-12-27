module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Utilities

import HsExpr
import HsBinds
import SrcLoc

exportMatch :: HsName n => Exporter (Located (Match n (LHsExpr n)))
exportAlternative :: HsName n => Exporter (Located (Match n (LHsExpr n)))
exportCaseRhss :: HsName n => Exporter (Located (GRHS n (LHsExpr n)))
exportLocalBinds :: HsName n => Exporter (LHsLocalBinds n)