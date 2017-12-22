module Language.Haskell.Heed.Export.Literals where
 
import SrcLoc
import HsLit

import Language.Haskell.Heed.Export.Utilities

exportMonoLiteral :: Located HsLit -> TrfType ()
exportMonoLiteral (L l (HsChar _ ch)) = export "Literal" "Character" l []
exportMonoLiteral (L l (HsCharPrim _ ch)) = export "Literal" "PrimitiveCharacter" l []
exportMonoLiteral (L l (HsString _ str)) = export "Literal" "String" l []
exportMonoLiteral (L l (HsStringPrim _ str)) = export "Literal" "PrimitiveString" l []
exportMonoLiteral (L l (HsIntPrim _ i)) = export "Literal" "PrimitiveInt" l []
exportMonoLiteral (L l (HsWordPrim _ i)) = export "Literal" "Word" l []
exportMonoLiteral (L l (HsInt64Prim _ i)) = export "Literal" "PrimitiveInt64" l []
exportMonoLiteral (L l (HsWord64Prim _ i)) = export "Literal" "PrimitiveWord64" l []
exportMonoLiteral (L l (HsFloatPrim frac)) = export "Literal" "PrimitiveFloat" l []
exportMonoLiteral (L l (HsDoublePrim frac)) = export "Literal" "PrimitiveDouble" l []
 
exportPolyLiteral :: Located OverLitVal -> TrfType ()
exportPolyLiteral (L l (HsIntegral _ i)) = export "Literal" "Integral" l []
exportPolyLiteral (L l (HsFractional frac)) = export "Literal" "Fractional" l []
exportPolyLiteral (L l (HsIsString _ str)) = export "Literal" "OverloadedString" l []
