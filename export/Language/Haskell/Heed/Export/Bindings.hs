{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Utilities

import HsBinds
import HsExpr
import SrcLoc

exportBinding :: HsName n => Located (HsBind n) -> TrfType ()
exportBinding (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _)) =
  export "Bind" "Fun" l [ "bind_matches" .-> mapM_ exportMatch matches ]

exportMatch :: forall n . HsName n => Located (Match n (LHsExpr n)) -> TrfType ()
exportMatch (L l (Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert "Match" "Match" l
  defining $ goInto id "match_name" $ exportNameOrRdrName @n (mc_fun name)
  addToScope (combineLocated pats) $ do
   goInto id "match_pats" $ mapM_ exportPattern pats
   goInto id "match_rhss" $ mapM_ exportRhss rhss

exportRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
exportRhss (L l (GRHS [] body)) =
  export "Rhs" "Unguarded" l [ "rhs_body" .-> exportExpression body ]
