{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Names where

import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Schema

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
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
  exportFieldOccName (L _ (FieldOcc rdr _)) = exportName rdr
  exportAmbiguous _ (L l ambiguous) = exportName (L l $ rdrNameAmbiguousFieldOcc ambiguous)

instance HsName GHC.Name where
  exportName (L l n) = writeName l n
  exportOperator (L l n) = writeName l n
  exportNameOrRdrName exporter = exporter
  exportFieldOccName (L l (FieldOcc _ name)) = exportName (L l name)

  exportAmbiguous exporter (L l (Unambiguous rdr pr)) = exporter (L l pr)
  exportAmbiguous exporter (L l (Ambiguous rdr _))
    = do sc <- asks scope
         case sc of Just scope -> tell (ExportStore [ (l, scope) ] [])
                    Nothing -> return ()

instance HsName Id where
  exportName (L l n) = do when (isRecordSelector n) $ do
                            scopeId <- asks (lookup l . ambiguousNames)
                            maybe (return ()) (doWriteName l (idName n) . Just) scopeId
                          writeType l n
  exportOperator (L l n) = do when (isRecordSelector n) $ writeName l (idName n)
                              writeType l n
  exportNameOrRdrName exporter = exporter
  exportFieldOccName (L l (FieldOcc _ n)) = writeType l n
  exportAmbiguous exporter (L l (Unambiguous _ pr)) = exporter (L l pr)
  exportAmbiguous exporter (L l (Ambiguous _ pr)) = exporter (L l pr)

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

