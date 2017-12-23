{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Names (exportOperator) where

import Language.Haskell.Heed.Export.Utilities hiding (Node(..))
import Language.Haskell.Heed.Export.Utilities (Node(Name, Operator))

import Control.Monad
import RdrName
import Data.Data
import qualified Name as GHC
import Name (isSymOcc, occName)
import HsTypes
import SrcLoc

data Name = Normal | Paren
  deriving Data

data Operator = NormalOp | Backtick
  deriving Data

instance HsName RdrName where
  exportName (L l _) = void $ writeInsert Name Normal l
  exportNameOrRdrName = exportName
  exportFieldOccName (L _ (FieldOcc rdr _)) = exportName rdr

instance HsName GHC.Name where
  exportName (L l n) = writeName l n
  exportNameOrRdrName = exportName
  exportFieldOccName (L l (FieldOcc _ name)) = exportName (L l name)

exportOperator :: HsName n => Located n -> TrfType ()
exportOperator (L l n)
  | isSymOcc (occName n) = void $ writeInsert Operator NormalOp l
  | otherwise            = void $ writeInsert Operator Backtick l