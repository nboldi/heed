{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Modules where

import Language.Haskell.Heed.Export.Declarations
import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import Bag
import HsSyn
import SrcLoc

exportRnModule :: HsName n => Exporter (HsGroup n, [LImportDecl n], Maybe [LIE n], Maybe LHsDocString)
exportRnModule (gr,_,_,_) = do
  export Module noSrcSpan [ exportDeclarationGroup gr ]


exportModule :: HsName n => Exporter (Located (HsModule n))
exportModule (L l (HsModule _ _ _ decls _ _)) =
  export Module l [ mapM_ exportDeclaration decls ]