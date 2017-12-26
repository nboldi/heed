{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import HsBinds
import HsExpr hiding (Match)
import qualified HsExpr as GHC
import SrcLoc

exportBinding :: HsName n => Located (HsBind n) -> TrfType ()
exportBinding (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _)) =
  export FunctionB l [ mapM_ exportMatch matches ]

exportMatch :: forall n . HsName n => Located (GHC.Match n (LHsExpr n)) -> TrfType ()
exportMatch (L l (GHC.Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert Match l
  defining $ goInto id 1 $ exportNameOrRdrName @n (mc_fun name)
  addToScope (combineLocated pats) $ do
   goInto id 2 $ mapM_ exportPattern pats
   goInto id 3 $ mapM_ exportRhss rhss

exportAlternative :: forall n . HsName n => Located (GHC.Match n (LHsExpr n)) -> TrfType ()
exportAlternative (L l (GHC.Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert Alternative l
  addToScope (combineLocated pats) $ do
   goInto id 1 $ mapM_ exportPattern pats
   goInto id 2 $ mapM_ exportRhss rhss


exportRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
exportRhss (L l (GRHS [] body)) =
  export Unguarded l [ exportExpression body ]

exportCaseRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
exportCaseRhss (L l (GRHS [] body)) =
  export UnguardedC l [ exportExpression body ]

