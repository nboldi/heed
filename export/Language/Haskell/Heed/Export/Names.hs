{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.Heed.Export.Names (exportOperator, exportImplicitName) where

import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Control.Monad
import RdrName
import Data.Data
import qualified Name as GHC
import Name (isSymOcc, occName)
import HsTypes
import SrcLoc
import Id

instance IsRdrName RdrName where
  toRdrName = id

instance IsRdrName GHC.Name where
  toRdrName = nameRdrName

instance IsRdrName Id where
  toRdrName = nameRdrName . idName

instance HsName RdrName where
  exportName (L l _) = void $ writeInsert Normal l
  exportNameOrRdrName = exportName
  exportFieldOccName (L _ (FieldOcc rdr _)) = exportName rdr

instance HsName GHC.Name where
  exportName (L l n) = writeName l n
  exportNameOrRdrName = exportName
  exportFieldOccName (L l (FieldOcc _ name)) = exportName (L l name)

exportImplicitName :: Located HsIPName -> TrfType ()
exportImplicitName = undefined

exportOperator :: HsName n => Located n -> TrfType ()
exportOperator (L l n)
  | isSymOcc (occName n) = void $ writeInsert NormalOp l
  | otherwise            = void $ writeInsert Backtick l


