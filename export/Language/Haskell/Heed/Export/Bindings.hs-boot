module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Utilities

import HsExpr
import HsBinds
import SrcLoc

exportMatch :: HsName n => Located (Match n (LHsExpr n)) -> TrfType ()
exportAlternative :: HsName n => Located (Match n (LHsExpr n)) -> TrfType ()
exportCaseRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
exportLocalBinds :: HsName n => LHsLocalBinds n -> TrfType ()