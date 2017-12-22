{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Utilities

import HsBinds
import HsExpr
import SrcLoc

trfBind :: HsName n => Located (HsBind n) -> TrfType ()
trfBind (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _)) =
  export "Bind" "Fun" l [ "bind_matches" .-> mapM_ trfMatch matches ]

trfMatch :: forall n . HsName n => Located (Match n (LHsExpr n)) -> TrfType ()
trfMatch (L l (Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert "Match" "Match" l
  defining $ goInto id "match_name" $ trfNameOrRdrName @n (mc_fun name)
  addToScope (combineLocated pats) $ do
   goInto id "match_pats" $ mapM_ trfPat pats
   goInto id "match_rhss" $ mapM_ trfRhss rhss

trfRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
trfRhss (L l (GRHS [] body)) =
  export "Rhs" "Unguarded" l [ "rhs_body" .-> trfExpr body ]
