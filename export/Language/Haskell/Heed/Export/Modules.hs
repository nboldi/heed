{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Modules where

import Language.Haskell.Heed.Export.Declarations
import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities

import Data.Data
import Bag
import HsSyn
import SrcLoc

data Module = MModule deriving Data

exportRnModule :: HsName n => (HsGroup n, [LImportDecl n], Maybe [LIE n], Maybe LHsDocString) -> TrfType ()
exportRnModule (gr,_,_,_) = do
  let binds = case hs_valds gr of ValBindsOut bindGroups _ -> unionManyBags (map snd bindGroups)
  id <- writeInsert Module MModule noSrcSpan
  addToScope (combineLocated $ bagToList binds)
    $ goInto id 1 $ mapM_ exportBinding (bagToList binds)

exportModule :: HsName n => Located (HsModule n) -> TrfType ()
exportModule (L l (HsModule _ _ _ decls _ _)) =
  export Module MModule l [ mapM_ exportDeclaration decls ]