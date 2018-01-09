{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-} -- export functions must be total
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Heed.Export.Types where

import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Kinds
import Language.Haskell.Heed.Export.Literals
import {-# SOURCE #-} Language.Haskell.Heed.Export.Templates
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Schema

import Control.Monad
import HsTypes
import BasicTypes as GHC
import HsExpr
import SrcLoc
import Outputable
import PlaceHolder(OutputableBndrId(..))

exportType :: HsName n => Exporter (Located (HsType n))
exportType (L l (HsForAllTy [] typ)) = exportType typ
exportType (L l (HsForAllTy bndrs typ))
  = export ForallT l [ defining (mapM_ exportTypeVar bndrs), exportType typ ]
exportType (L l (HsQualTy (L _ []) typ)) = exportType typ
exportType (L l (HsQualTy ctx typ)) = export ContextT l [ exportContext ctx, exportType typ ]
exportType (L l (HsTyVar _ name)) = export VariableT l [ exportName name ]
exportType (L l (HsAppsTy apps)) | Just (headT, args, _) <- getAppsTyHead_maybe apps
  = export ApplicationT l [ exportType headT, mapM_ exportType args ]
exportType (L l (HsAppTy t1 t2)) = export ApplicationT l [ exportType t1, exportType t2 ]
exportType (L l (HsFunTy t1 t2)) = export FunctionT l [ exportType t1, exportType t2 ]
exportType (L l (HsListTy typ)) = export ListT l [ exportType typ ]
exportType (L l (HsPArrTy typ)) = export ParallelArrayT l [ exportType typ ]
exportType (L l (HsTupleTy HsBoxedOrConstraintTuple typs)) = export TupleT l [ mapM_ exportType typs ]
exportType (L l (HsTupleTy HsBoxedTuple typs)) = export TupleT l [ mapM_ exportType typs ]
exportType (L l (HsTupleTy HsUnboxedTuple typs)) = export UnboxedTupleT l [ mapM_ exportType typs ]
exportType (L l (HsOpTy t1 op t2))
  = export InfixT l [ exportType t1, exportOperator op, exportType t2 ]
exportType (L l (HsParTy typ)) = export ParenT l [ exportType typ ]
exportType (L l (HsKindSig typ kind)) = export KindedT l [ exportType typ, exportKind kind ]
exportType (L l (HsSpliceTy qq@(HsQuasiQuote {}) _)) = export QuasiQouteT l [ exportQuasiQuotation (L l qq) ]
exportType (L l (HsSpliceTy splice _)) = export SpliceT l [ exportSplice (L l splice) ]
exportType (L l (HsBangTy (HsSrcBang _ SrcUnpack _) typ)) = export UnpackT l [ exportType typ ]
exportType (L l (HsBangTy (HsSrcBang _ SrcNoUnpack _) typ)) = export NoUnpackT l [ exportType typ ]
exportType (L l (HsBangTy (HsSrcBang _ _ SrcStrict) typ)) = export BangT l [ exportType typ ]
exportType (L l (HsBangTy (HsSrcBang _ _ SrcLazy) typ)) = export LazyT l [ exportType typ ]
exportType (L l pt@(HsExplicitListTy {})) = export PromotedT l [ exportPromoted exportType (L l pt) ]
exportType (L l pt@(HsExplicitTupleTy {})) = export PromotedT l [ exportPromoted exportType (L l pt) ]
exportType (L l pt@(HsTyLit {})) = export PromotedT l [ exportPromoted exportType (L l pt) ]
exportType (L l (HsWildCardTy _)) = export WildcardT l []
exportType (L l (HsSumTy types)) = export SumT l [ mapM_ exportType types ]
exportType (L l t) = exportError "type" t

exportTypeVar :: HsName n => Exporter (Located (HsTyVarBndr n))
exportTypeVar (L l (UserTyVar name)) = export NormalTV l [ exportName name ]
exportTypeVar (L l (KindedTyVar name kind))
  = export KindedTV l [ exportName name, exportKindSignature (Just kind) ]

exportContext :: HsName n => Exporter (Located (HsContext n))
exportContext (L l ctx) = export Context l [ mapM_ exportPredicate ctx ]


exportPredicate :: HsName n => Exporter (LHsType n)
exportPredicate = exportPredicate' . cleanHsType

exportPredicate' :: HsName n => Exporter (LHsType n)
exportPredicate' (L l (HsParTy t)) = exportPredicate t
exportPredicate' (L l (HsOpTy left op right))
  = export InfixPredicate l [ exportType left, exportOperator op, exportType right ]
exportPredicate' (L l (HsTupleTy _ tys)) = export TuplePredicate l [ mapM_ exportPredicate tys ]
exportPredicate' (L l (HsWildCardTy _)) = export WildcardPredicate l []
exportPredicate' (L l (unappType -> (args, HsTyVar _ name)))
  = export ClassPredicate l [ exportName name, mapM_ exportType args ]
exportPredicate' (L l (unappType -> ([], HsEqTy t1 t2)))
  = export TypeEqPredicate l [ exportType t1, exportType t2 ]
exportPredicate' (L l (unappType -> ([], HsIParamTy name typ)))
  = export ImplicitPredicate l [ defining (exportImplicitName name), exportType typ ]
exportPredicate' (L l t) = exportError "predicate" t

unappType :: HsType n -> ([LHsType n], HsType n)
unappType (HsAppTy (L l ft) at) = case unappType ft of (args, base) -> (args++[at], base)
unappType t                     = ([], t)

-- | Tries to simplify the type that has HsAppsTy before renaming. Does not always provide the correct form.
-- Treats each operator as if they are of equivalent precedence and always left-associative.
cleanHsType :: OutputableBndrId n => LHsType n -> LHsType n
-- for some reason * is considered infix
cleanHsType (L l (HsAppsTy apps)) = guessType apps
  where guessType :: OutputableBndrId n => [LHsAppType n] -> LHsType n
        guessType (L l (HsAppInfix n) : rest) -- must be a prefix actually
          = guessType' (L l (HsTyVar NotPromoted n)) rest
        guessType (L _ (HsAppPrefix t) : rest) = guessType' t rest
        guessType [] = error $ "guessType: empty: " ++ showSDocUnsafe (ppr apps)

        guessType' :: OutputableBndrId n => LHsType n -> [LHsAppType n] -> LHsType n
        guessType' fun (L _ (HsAppPrefix t) : rest) = guessType' (hsAppTy fun t) rest
        guessType' fun (L l (HsAppInfix n) : rest)
          -- TODO: find a better check
          | showSDocUnsafe (ppr n) == "*" = guessType' (hsAppTy fun (L l (HsTyVar NotPromoted n))) rest
        guessType' left (L _ (HsAppInfix n) : right) = hsOpTy left n (guessType right)
        guessType' t [] = t

        hsAppTy :: LHsType n -> LHsType n -> LHsType n
        hsAppTy t1 t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsAppTy t1 t2

        hsOpTy :: LHsType n -> Located n -> LHsType n -> LHsType n
        hsOpTy t1 n t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsOpTy t1 n t2
cleanHsType t = t
