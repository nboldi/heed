module Language.Haskell.Heed.Export.Expressions where

import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Utilities

import HsExpr
import SrcLoc

trfExpr :: HsName n => Located (HsExpr n) -> TrfType ()
trfExpr (L l (OpApp e1 (L _ (HsVar op)) _ e2)) = 
  export "Expr" "InfixApp" l [ "expr_lhs" .-> trfExpr e1
                             , "expr_op" .-> trfName op
                             , "expr_rhs" .-> trfExpr e2 ]
trfExpr (L l (HsVar name)) =
  export "Expr" "Var" l [ "expr_name" .-> trfName name ]