module Language.Haskell.Heed.Export.Names where

import Language.Haskell.Heed.Export.Utilities

import Control.Monad
import RdrName
import Name
import SrcLoc

instance HsName RdrName where
  trfName (L l _) = void $ writeInsert "Name" "Name" l
  trfNameOrRdrName = trfName

instance HsName Name where
  trfName (L l n) = writeName l n
  trfNameOrRdrName = trfName