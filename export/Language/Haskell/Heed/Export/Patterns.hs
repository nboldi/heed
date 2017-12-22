module Language.Haskell.Heed.Export.Patterns where

import {-# SOURCE #-} Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Utilities

import HsPat
import SrcLoc

trfPat :: HsName n => Located (Pat n) -> TrfType ()
trfPat (L l (VarPat name)) =
  export "Pattern" "Variable" l [ "pat_name" .-> defining (trfName name) ]

