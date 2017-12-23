{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Declarations where

import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities

import Data.Data
import HsDecls
import SrcLoc

data Declaration = DBinding deriving Data

exportDeclaration :: HsName n => Located (HsDecl n) -> TrfType ()
exportDeclaration (L l (ValD bind)) =
  export Declaration DBinding l [ exportBinding (L l bind) ]
