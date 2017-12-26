{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Kinds where

import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import HsTypes
import RdrName
import Name
import SrcLoc

exportKindSignature :: HsName n => Maybe (LHsKind n) -> TrfType ()
exportKindSignature (Just kind)
  = export KindSignature (getLoc kind) [ exportKind kind ]
exportKindSignature Nothing = return ()

exportKind :: HsName n => LHsKind n -> TrfType ()
-- exportKind (L l (HsTyVar _ (unLoc -> Exact n)))
--   | isWiredInName n && occNameString (nameOccName n) == "*" = export "Kind" "StarKind" l []
--   | isWiredInName n && occNameString (nameOccName n) == "#" = export "Kind" "UnboxKind" l []
exportKind (L l (HsParTy kind)) = export ParenK l [ exportKind kind ]
exportKind (L l (HsFunTy kArg kRes)) = export FunctionK l [ exportKind kArg, exportKind kRes ]
exportKind (L l (HsAppTy kFun kArg))
  = export ApplicationK l [ exportKind kFun, exportKind kArg ]
exportKind (L l (HsOpTy lhs op rhs))
  = export InfixApplicationK l [ exportKind lhs, exportOperator op, exportKind rhs ]


--   trfKind'' (HsAppTy k1 k2) = AST.UAppKind <$> trfKind k1 <*> trfKind k2
--   trfKind'' (HsOpTy k1 op k2) = AST.UInfixAppKind <$> trfKind k1 <*> trfOperator op <*> trfKind k2
--   trfKind'' (HsTyVar _ kv) = transformingPossibleVar kv (AST.UVarKind <$> trfName kv)
--   trfKind'' (HsListTy kind) = AST.UListKind <$> trfKind kind
--   trfKind'' (HsTupleTy _ kinds) = AST.UTupleKind <$> makeList ", " atTheStart (mapM trfKind kinds)
--   trfKind'' (HsAppsTy [unLoc -> HsAppPrefix t]) = trfKind' (unLoc t)
--   trfKind'' (HsAppsTy [unLoc -> HsAppInfix n]) = AST.UVarKind <$> trfName n
--   trfKind'' pt@(HsExplicitListTy {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt)
--   trfKind'' pt@(HsExplicitTupleTy {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt)
--   trfKind'' pt@(HsTyLit {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt)
--   trfKind'' t = AST.UTypeKind <$> annContNoSema (trfType' t)
--
-- trfPromoted' :: (TransformName n r, HasNoSemanticInfo (Dom r) a)
--                   => (HsType n -> Trf (a (Dom r) RangeStage)) -> HsType n -> Trf (AST.UPromoted a (Dom r) RangeStage)
-- trfPromoted' _ (HsTyLit (HsNumTy _ int)) = pure $ AST.UPromotedInt int
-- trfPromoted' _ (HsTyLit (HsStrTy _ str)) = pure $ AST.UPromotedString (unpackFS str)
-- trfPromoted' _ (HsTyVar _ name) = AST.UPromotedCon <$> trfName name
-- trfPromoted' f (HsExplicitListTy _ _ elems) = AST.UPromotedList <$> between AnnOpenS AnnCloseS (trfAnnList ", " f elems)
-- trfPromoted' f (HsExplicitTupleTy _ elems) = AST.UPromotedTuple <$> between AnnOpenP AnnCloseP (trfAnnList ", " f elems)
-- trfPromoted' _ t = unhandledElement "promoted type/kind" t