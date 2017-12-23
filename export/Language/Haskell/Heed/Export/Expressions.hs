{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Expressions (exportExpression) where

import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Utilities

import Data.Data (Data(..))
import HsExpr
import SrcLoc

data Expression = Var
                | InfixApp
  deriving (Show, Data)

exportExpr = export Expression

exportExpression :: HsName n => Located (HsExpr n) -> TrfType ()
exportExpression (L l (OpApp e1 (L _ (HsVar op)) _ e2))
  = exportExpr InfixApp l [ exportExpression e1, exportName op, exportExpression e2 ]
exportExpression (L l (HsVar name))
  = exportExpr Var l [ exportName name ]