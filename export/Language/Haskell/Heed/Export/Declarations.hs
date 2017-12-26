{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Declarations where

import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import HsDecls
import SrcLoc

exportDeclaration :: HsName n => Located (HsDecl n) -> TrfType ()
exportDeclaration (L l (ValD bind)) = export BindingD l [ exportBinding (L l bind) ]
