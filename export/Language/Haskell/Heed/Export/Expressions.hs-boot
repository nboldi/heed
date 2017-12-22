module Language.Haskell.Heed.Export.Expressions where

import Language.Haskell.Heed.Export.Utilities
import HsExpr
import SrcLoc

exportExpression :: HsName n => Located (HsExpr n) -> TrfType ()
