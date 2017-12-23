{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Utilities

import Data.Data
import HsBinds
import HsExpr hiding (Match)
import qualified HsExpr as GHC
import SrcLoc

data Bind = Function deriving Data
data Match = M deriving Data
data Rhs = Unguarded deriving Data

exportBinding :: HsName n => Located (HsBind n) -> TrfType ()
exportBinding (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _)) =
  export Binding Function l [ mapM_ exportMatch matches ]

exportMatch :: forall n . HsName n => Located (GHC.Match n (LHsExpr n)) -> TrfType ()
exportMatch (L l (GHC.Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert Match M l
  defining $ goInto id 1 $ exportNameOrRdrName @n (mc_fun name)
  addToScope (combineLocated pats) $ do
   goInto id 2 $ mapM_ exportPattern pats
   goInto id 3 $ mapM_ exportRhss rhss

exportRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
exportRhss (L l (GRHS [] body)) =
  export Rhs Unguarded l [ exportExpression body ]
