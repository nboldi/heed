module Language.Haskell.Heed.Export.Types where

import Language.Haskell.Heed.Export.Utilities

import HsTypes
import SrcLoc

exportType :: HsName n => Exporter (Located (HsType n))