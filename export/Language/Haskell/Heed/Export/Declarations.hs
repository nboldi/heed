module Language.Haskell.Heed.Export.Declarations where

import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities

import HsDecls
import SrcLoc

exportDeclaration :: HsName n => Located (HsDecl n) -> TrfType ()
exportDeclaration (L l (ValD bind)) =
  export "Decl" "Bind" l [ "decl_bind" .-> exportBinding (L l bind) ]
