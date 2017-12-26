{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Literals where

import Data.Data
import SrcLoc
import HsLit

import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

exportMonoLiteral :: Located HsLit -> TrfType ()
exportMonoLiteral (L l (HsChar _ ch)) = export Character l []
exportMonoLiteral (L l (HsCharPrim _ ch)) = export PrimitiveCharacter l []
exportMonoLiteral (L l (HsString _ str)) = export String l []
exportMonoLiteral (L l (HsStringPrim _ str)) = export PrimitiveString l []
exportMonoLiteral (L l (HsIntPrim _ i)) = export PrimitiveInt l []
exportMonoLiteral (L l (HsWordPrim _ i)) = export Word l []
exportMonoLiteral (L l (HsInt64Prim _ i)) = export PrimitiveInt64 l []
exportMonoLiteral (L l (HsWord64Prim _ i)) = export PrimitiveWord64 l []
exportMonoLiteral (L l (HsFloatPrim frac)) = export PrimitiveFloat l []
exportMonoLiteral (L l (HsDoublePrim frac)) = export PrimitiveDouble l []

exportPolyLiteral :: Located OverLitVal -> TrfType ()
exportPolyLiteral (L l (HsIntegral _ i)) = export Integral l []
exportPolyLiteral (L l (HsFractional frac)) = export Fractional l []
exportPolyLiteral (L l (HsIsString _ str)) = export OverloadedString l []
