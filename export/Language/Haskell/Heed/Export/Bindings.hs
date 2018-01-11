{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Heed.Export.Bindings where

import Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Types
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Schema as Schema

import Control.Monad.IO.Class
import Data.Data
import HsBinds
import Bag
import BasicTypes as GHC
import HsTypes
import HsExpr hiding (Match)
import qualified HsExpr as GHC
import SrcLoc

exportBinding :: HsName n => Exporter (Located (HsBind n))
-- A value binding with a strictness annotation
exportBinding (L l (FunBind { fun_id = id
                            , fun_matches = MG { mg_alts = L _ [L _ (GHC.Match { m_ctxt = FunRhs { mc_strictness = SrcStrict }
                                                                               , m_pats = []
                                                                               , m_grhss = GRHSs [rhss] locals })]} }))
  = export SimpleB l [ export BangP (getLoc id) [ export VariableP (getLoc id) [ defining (exportName id) ] ]
                     , exportRhss rhss
                     , exportLocalBinds locals
                     ]
-- A value binding (not a function)
exportBinding (L l (FunBind { fun_id = id
                            , fun_matches = MG { mg_alts = L _ [L _ (GHC.Match { m_pats = []
                                                                               , m_grhss = GRHSs [rhss] locals })]} }))
  = export SimpleB l [ export VariableP (getLoc id) [ defining (exportName id) ]
                     , exportRhss rhss
                     , exportLocalBinds locals ]
exportBinding (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _))
  = export FunctionB l [ mapM_ exportMatch matches ]
exportBinding (L l (PatBind pat (GRHSs rhs locals) _ _ _))
  = export SimpleB l [ exportPattern pat
                     , mapM_ exportRhss rhs
                     , exportLocalBinds locals ]
exportBinding (L l (VarBind {})) = return () -- not a source binding
exportBinding (L l b@(AbsBinds {})) = mapM_ exportBinding (abs_binds b)
exportBinding (L l b@(AbsBindsSig {})) = exportBinding (abs_sig_bind b)
exportBinding (L l bind) = exportError "binding" bind --  Pattern synonym bindings should be recognized on the declaration level

exportMatch :: HsName n => Exporter (Located (GHC.Match n (LHsExpr n)))
exportMatch = gExportMatch exportExpression

gExportMatch :: forall n e . HsName n => Exporter e -> Exporter (Located (GHC.Match n e))
gExportMatch exportExpr (L l (GHC.Match id pats _ (GRHSs rhss locBinds))) = do
  node <- writeInsert Match l
  defining $ goInto node 1
    $ case id of FunRhs name GHC.Prefix _ -> exportNameOrRdrName @n exportName name
                 FunRhs name GHC.Infix _  -> exportNameOrRdrName @n exportOperator name
                 _                        -> return ()
  addToScope (combineLocated pats) $ do
    goInto node 2 $ mapM_ exportPattern pats
    goInto node 3 $ mapM_ (gExportRhss exportExpr) rhss
    goInto node 4 $ exportLocalBinds locBinds

exportRhss :: HsName n => Exporter (Located (GRHS n (LHsExpr n)))
exportRhss = gExportRhss exportExpression

gExportRhss :: HsName n => Exporter e -> Exporter (Located (GRHS n e))
gExportRhss exportExpr (L l (GRHS [] body)) = export UnguardedRhs l [ exportExpr body ]
gExportRhss exportExpr (L l (GRHS guards body)) = export GuardedRhss l [ scopedSequence exportRhsGuard guards
                                                                       , exportExpr body ]

exportRhsGuard :: HsName n => Exporter (LStmt n (LHsExpr n))
exportRhsGuard (L l (BindStmt pat body _ _ _))
  = export GuardBind l [ exportPattern pat, exportExpression body ]
exportRhsGuard (L l (BodyStmt body _ _ _)) = export GuardBody l [ exportExpression body ]
exportRhsGuard (L l (LetStmt binds)) = export GuardLet l [ exportLocalBinds binds ]
exportRhsGuard (L l stmt) = exportError "rhs guard" stmt

exportLocalBinds :: HsName n => Exporter (LHsLocalBinds n)
exportLocalBinds (L l (HsValBinds (ValBindsIn binds sigs)))
  = addToScope l $ export LocalBindings l [ mapM_ exportBinding (bagToList binds) >> mapM_ exportLocalSig sigs ]
exportLocalBinds (L l (HsValBinds (ValBindsOut binds sigs)))
  = addToScope l $ export LocalBindings l [ mapM_ exportBinding (concatMap (bagToList . snd) binds) >> mapM_ exportLocalSig sigs ]
exportLocalBinds bind@(L l (HsIPBinds (IPBinds binds _))) = addToScope l $ mapM_ exportIPBind binds
exportLocalBinds (L l EmptyLocalBinds) = return ()

exportLocalSig :: HsName n => Exporter (Located (Sig n))
exportLocalSig ts@(L l (TypeSig {})) = export LocalTypeSignature l [exportTypeSignature ts]
exportLocalSig (L l (FixSig fs)) = export LocalFixitySignature l [exportFixitySignature (L l fs)]
exportLocalSig (L l (InlineSig name prag))
  = export LocalPragma l [ export Schema.InlinePragma l [exportName name, exportInlinePragma (L l prag)] ]
exportLocalSig (L l ls) = exportError "local signature" ls

exportTypeSignature :: HsName n => Exporter (Located (Sig n))
exportTypeSignature (L l (TypeSig names typ))
  = export TypeSignature l [ mapM_ exportName names, exportType (hsib_body $ hswc_body typ) ]
exportTypeSignature (L l ts) = exportError "type signature" ts


exportFixitySignature :: HsName n => Exporter (Located (FixitySig n))
exportFixitySignature (L l (FixitySig names (Fixity _ prec InfixL)))
  = export FixitySignatureLeft l [ writeIntAttribute prec ]
exportFixitySignature (L l (FixitySig names (Fixity _ prec InfixR)))
  = export FixitySignatureRight l [ writeIntAttribute prec ]
exportFixitySignature (L l (FixitySig names (Fixity _ prec InfixN)))
  = export FixitySignature l [ writeIntAttribute prec ]

exportIPBind :: HsName n => Exporter (Located (IPBind n))
exportIPBind (L l (IPBind (Left (L nameL ipname)) expr))
  = export SimpleB l [ export VariableP nameL [ exportImplicitName (L nameL ipname) ]
                     , export UnguardedRhs (getLoc expr) [ exportExpression expr ] ]
exportIPBind (L l ip@(IPBind (Right _) _))
  = exportError "implicit binding" ip -- called on typechecked AST


exportInlinePragma :: Exporter (Located GHC.InlinePragma)
exportInlinePragma (L l (GHC.InlinePragma _ Inlinable _ phase _))
  = export InlinablePragma l [ exportPhase (L l phase) ]
exportInlinePragma (L l (GHC.InlinePragma _ NoInline _ _ _)) = export NoInlinePragma l []
exportInlinePragma (L l (GHC.InlinePragma _ Inline _ phase cl))
  = export Schema.InlinePragma l [ exportPhase (L l phase), writeBoolAttribute (cl == ConLike) ]

exportPhase :: Exporter (Located Activation)
exportPhase (L l AlwaysActive) = return ()
exportPhase (L l NeverActive) = export NoPhase l []
exportPhase (L l (ActiveAfter _ pn)) = export AfterPhase l [ writeIntAttribute pn ]
exportPhase (L l (ActiveBefore _ pn)) = export BeforePhase l [ writeIntAttribute pn ]

