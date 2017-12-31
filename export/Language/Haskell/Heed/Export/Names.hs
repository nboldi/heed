{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Names (exportOperator, exportImplicitName, exportAmbiguousOperator, exportAmbiguousFieldName) where

import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Control.Monad
import Control.Monad.IO.Class
import RdrName
import Data.List
import Data.Data
import qualified Name as GHC
import Name (isSymOcc, occName)
import HsTypes
import FastString
import SrcLoc
import Id
import Outputable

instance IsRdrName RdrName where
  toRdrName = id

instance IsRdrName GHC.Name where
  toRdrName = nameRdrName

instance IsRdrName Id where
  toRdrName = nameRdrName . idName

instance HsName RdrName where
  exportName (L l n)
    | isSymOcc (occName n)
    = export ParenName l [ exportQualifiers pLoc nameStr, exportUnqualified pLoc nameStr ]
    | otherwise
    = export NormalName l [ exportQualifiers l nameStr, exportUnqualified l nameStr ]
    where nameStr = showSDocUnsafe $ ppr n
          pLoc = updateStart (updateCol (+1)) $ updateEnd (updateCol (subtract 1)) l
  exportOperator (L l n)
    | isSymOcc (occName n)
    = export NormalOperator l [ exportQualifiers l nameStr, exportUnqualified l nameStr ]
    | otherwise
    = export BacktickOperator l [ exportQualifiers pLoc nameStr, exportUnqualified pLoc nameStr ]
    where nameStr = showSDocUnsafe $ ppr n
          pLoc = updateStart (updateCol (+1)) $ updateEnd (updateCol (subtract 1)) l

  exportNameOrRdrName exporter = exporter
  exportRnName exporter rdr _ = exporter rdr
  exportFieldOccName (L _ (FieldOcc rdr _)) = exportName rdr

instance HsName GHC.Name where
  exportName (L l n) = writeName l n
  exportOperator (L l n) = writeName l n
  exportNameOrRdrName exporter = exporter
  exportRnName exporter _ = exporter
  exportFieldOccName (L l (FieldOcc _ name)) = exportName (L l name)

instance HsName Id where
  exportName (L l n) = liftIO (putStrLn $ "exportName: " ++ showSDocUnsafe (ppr n)) >> writeType l n
  exportOperator (L l n) = liftIO (putStrLn $ "exportOperator: " ++ showSDocUnsafe (ppr n)) >> writeType l n
  exportNameOrRdrName exporter = exporter
  exportRnName exporter _ = exporter
  exportFieldOccName (L l (FieldOcc _ name)) = exportName (L l name)


exportAmbiguousFieldName :: forall n . HsName n => Exporter (Located (AmbiguousFieldOcc n))
exportAmbiguousFieldName (L l (Unambiguous rdr pr)) = exportRnName @n exportName rdr (L l pr)
exportAmbiguousFieldName (L l (Ambiguous rdr _)) = exportName rdr

exportAmbiguousOperator :: forall n . HsName n => Exporter (Located (AmbiguousFieldOcc n))
exportAmbiguousOperator (L l (Unambiguous rdr pr)) = exportRnName @n exportOperator rdr (L l pr)
exportAmbiguousOperator (L l (Ambiguous rdr _)) = exportOperator rdr

exportImplicitName :: Exporter (Located HsIPName)
exportImplicitName (L l (HsIPName fs))
  = export ImplicitName l [ writeStringAttribute (unpackFS fs) ]

exportQualifiers :: SrcSpan -> Exporter String
exportQualifiers (RealSrcSpan sp) str
  = export Qualifiers (RealSrcSpan $ mkRealSrcSpan (realSrcSpanStart sp) qualEndPos)
      [ writeStringAttribute (take lastDot str) ]
  where lastDot = case elemIndices '.' str of inds@(_:_) -> last inds; [] -> -1
        qualEndPos = foldl advanceSrcLoc (realSrcSpanStart sp) (take lastDot str)
exportQualifiers sp _ = exportError "qualifiers" sp

exportUnqualified :: SrcSpan -> Exporter String
exportUnqualified (RealSrcSpan sp) str
  = export UnqualifiedName (RealSrcSpan $ mkRealSrcSpan qualStartPos (realSrcSpanEnd sp))
      [ writeStringAttribute (drop (lastDot + 1) str) ]
  where lastDot = case elemIndices '.' str of inds@(_:_) -> last inds; [] -> -1
        qualStartPos = foldl advanceSrcLoc (realSrcSpanStart sp) (take (lastDot + 1) str)
exportUnqualified sp _ = exportError "unqualified name" sp

