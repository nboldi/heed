module Language.Haskell.Heed.Export.Declarations where

import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities

import HsDecls
import SrcLoc

trfDecl :: HsName n => Located (HsDecl n) -> TrfType ()
trfDecl (L l (ValD bind)) =
  export "Decl" "Bind" l [ "decl_bind" .-> trfBind (L l bind) ]
