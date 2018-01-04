{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Kinds where

import Language.Haskell.Heed.Export.Names
import {-# SOURCE #-} Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Schema

import Data.Data
import HsTypes
import RdrName
import FastString
import Name
import SrcLoc

exportKindSignature :: HsName n => Exporter (Maybe (LHsKind n))
exportKindSignature (Just kind)
  = export KindSignature (getLoc kind) [ exportKind kind ]
exportKindSignature Nothing = return ()

exportKind :: HsName n => Exporter (LHsKind n)
exportKind (L l (HsTyVar _ (toRdrName . unLoc -> Exact n)))
  | isWiredInName n && occNameString (nameOccName n) == "*" = export StarK l []
  | isWiredInName n && occNameString (nameOccName n) == "#" = export UnboxK l []
exportKind (L l (HsParTy kind)) = export ParenK l [ exportKind kind ]
exportKind (L l (HsFunTy kArg kRes)) = export FunctionK l [ exportKind kArg, exportKind kRes ]
exportKind (L l (HsAppTy kFun kArg))
  = export ApplicationK l [ exportKind kFun, exportKind kArg ]
exportKind (L l (HsOpTy lhs op rhs))
  = export InfixApplicationK l [ exportKind lhs, exportOperator op, exportKind rhs ]
exportKind (L l (HsTyVar _ kv)) = export VarK l [ defining $ exportName kv ]
exportKind (L l (HsListTy kind)) = export ListK l [ exportKind kind ]
exportKind (L l (HsTupleTy _ kinds)) = export TupleK l [ mapM_ exportKind kinds ]
exportKind (L l (HsAppsTy [unLoc -> HsAppPrefix k])) = exportKind k
exportKind (L l (HsAppsTy [unLoc -> HsAppInfix n])) = export VarK l [ defining $ exportName n ]
exportKind (L l pt@(HsExplicitListTy {})) = export PromotedK l [ exportPromoted exportKind (L l pt) ]
exportKind (L l pt@(HsExplicitTupleTy {})) = export PromotedK l [ exportPromoted exportKind (L l pt) ]
exportKind (L l pt@(HsTyLit {})) = export PromotedK l [ exportPromoted exportKind (L l pt) ]
exportKind (L l t) = export TypeK l [ exportType (L l t) ]

exportPromoted :: HsName n => Exporter (LHsType n) -> Exporter (LHsType n)
exportPromoted _ (L l (HsTyLit (HsNumTy _ int))) = export PromotedInt l [ writeIntAttribute (fromIntegral int) ]
exportPromoted _ (L l (HsTyLit (HsStrTy _ str))) = export PromotedString l [ writeStringAttribute (unpackFS str) ]
exportPromoted exporter (L l (HsExplicitListTy _ _ elems)) = export PromotedList l [ mapM_ exporter elems ]
exportPromoted exporter (L l (HsExplicitTupleTy _ elems)) = export PromotedTuple l [ mapM_ exporter elems ]
exportPromoted _ (L l p) = exportError "promoted type/kind" p
