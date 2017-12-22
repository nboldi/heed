module Language.Haskell.Heed.Export.Expressions where

import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Utilities

import HsExpr
import SrcLoc

exportExpression :: HsName n => Located (HsExpr n) -> TrfType ()
exportExpression (L l (OpApp e1 (L _ (HsVar op)) _ e2)) = 
  export "Expr" "InfixApp" l [ "expr_lhs" .-> exportExpression e1
                             , "expr_op" .-> exportName op
                             , "expr_rhs" .-> exportExpression e2 ]
exportExpression (L l (HsVar name)) =
  export "Expr" "Var" l [ "expr_name" .-> exportName name ]