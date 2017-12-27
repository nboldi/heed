{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Literals where

import Data.Data
import SrcLoc
import HsLit
import FastString
import Data.ByteString.Char8 as BS
import BasicTypes

import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

exportMonoLiteral :: Exporter (Located HsLit)
exportMonoLiteral (L l (HsChar _ ch)) = export Character l [ writeStringAttribute [ch] ]
exportMonoLiteral (L l (HsCharPrim _ ch)) = export PrimitiveCharacter l [ writeStringAttribute [ch] ]
exportMonoLiteral (L l (HsString _ str)) = export String l [ writeStringAttribute (unpackFS str) ]
exportMonoLiteral (L l (HsStringPrim _ str)) = export PrimitiveString l [ writeStringAttribute (BS.foldr (:) "" str) ]
exportMonoLiteral (L l (HsIntPrim _ i)) = export PrimitiveInt l [ writeIntAttribute (fromIntegral i) ]
exportMonoLiteral (L l (HsWordPrim _ i)) = export Word l [ writeIntAttribute (fromIntegral i) ]
exportMonoLiteral (L l (HsInt64Prim _ i)) = export PrimitiveInt64 l [ writeIntAttribute (fromIntegral i) ]
exportMonoLiteral (L l (HsWord64Prim _ i)) = export PrimitiveWord64 l [ writeIntAttribute (fromIntegral i) ]
exportMonoLiteral (L l (HsFloatPrim frac)) = export PrimitiveFloat l [ writeFractionalAttribute (fromRational $ fl_value frac) ]
exportMonoLiteral (L l (HsDoublePrim frac)) = export PrimitiveDouble l [ writeFractionalAttribute (fromRational $ fl_value frac) ]

exportPolyLiteral :: Exporter (Located OverLitVal)
exportPolyLiteral (L l (HsIntegral _ i)) = export Integral l [ writeIntAttribute (fromIntegral i) ]
exportPolyLiteral (L l (HsFractional frac)) = export Fractional l [ writeFractionalAttribute (fromRational $ fl_value frac) ]
exportPolyLiteral (L l (HsIsString _ str)) = export OverloadedString l [ writeStringAttribute (unpackFS str) ]
