{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Declarations where

import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import HsDecls
import Bag
import HsSyn
import SrcLoc

exportDeclarationGroup :: HsName n => Exporter (HsGroup n)
exportDeclarationGroup gr = do
  let binds = case hs_valds gr of ValBindsOut bindGroups _ -> unionManyBags (map snd bindGroups)
  addToScope (combineLocated $ bagToList binds)
    $ goInto Nothing 1 $ mapM_ exportBinding (bagToList binds)

exportDeclaration :: HsName n => Exporter (Located (HsDecl n))
exportDeclaration (L l (ValD bind)) = export BindingD l [ exportBinding (L l bind) ]
