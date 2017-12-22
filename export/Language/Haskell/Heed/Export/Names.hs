module Language.Haskell.Heed.Export.Names where

import Language.Haskell.Heed.Export.Utilities

import Control.Monad
import RdrName
import Name
import HsTypes
import SrcLoc

instance HsName RdrName where
  exportName (L l _) = void $ writeInsert "Name" "Name" l
  exportNameOrRdrName = exportName
  exportFieldOccName (L _ (FieldOcc rdr _)) = exportName rdr

instance HsName Name where
  exportName (L l n) = writeName l n
  exportNameOrRdrName = exportName
  exportFieldOccName (L l (FieldOcc _ name)) = exportName (L l name)

exportOperator :: HsName n => Located n -> TrfType ()
exportOperator (L l n)
  | isSymOcc (occName n) = void $ writeInsert "Operator" "Normal" l
  | otherwise            = void $ writeInsert "Operator" "Backtick" l