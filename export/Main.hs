{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Main where

import Control.Monad.State
import Control.Monad.Reader
import Database.Selda
import Database.Selda.SQLite
import Data.Maybe
import qualified Data.Map as Map
import Data.Text (pack)
import System.Environment

import GHC
import Outputable
import SrcLoc
import Bag
import FastString
import Exception
 
import GHC.Paths ( libdir )
import DynFlags
import Name
import OccName

import Core
 
main :: IO ()
main = do args <- getArgs 
          case args of "no-export":_ -> example False
                       _             -> example True

fileName = "A.hs"
modName = "A"

example doExport = 
  runGhc (Just libdir) $ 
    withSQLite "haskell.db" $ 
      transaction $ do
        dflags <- liftGhc getSessionDynFlags
        void $ liftGhc $ setSessionDynFlags
          $ flip gopt_set Opt_KeepRawTokenStream dflags
        target <- liftGhc $ guessTarget fileName Nothing
        liftGhc $ setTargets [target]
        liftGhc $ load LoadAllTargets
        modSum <- liftGhc $ getModSummary $ mkModuleName modName
        p <- liftGhc $ parseModule modSum
        let (ghcTokens, ghcComments) = pm_annotations $ p
            tokenKeys = Map.assocs ghcTokens
        t <- liftGhc $ typecheckModule p
        
        when doExport $ do
          cleanDatabase

          insertTokens tokenKeys
          insertComments (concat $ Map.elems ghcComments)
          let es = initExportState True (ms_mod modSum)
          flip runReaderT es $ trfModule $ parsedSource p
          case renamedSource t of Just rs -> flip runReaderT (es {exportSyntax = False}) $ trfRnModule rs

cleanDatabase :: SeldaT Ghc ()
cleanDatabase = do
   tryDropTable tokens
   createTable tokens
   tryDropTable comments
   createTable comments
   tryDropTable nodes
   createTable nodes
   tryDropTable names
   createTable names
   tryDropTable scopes
   createTable scopes
   tryDropTable scopeNames
   createTable scopeNames

insertTokens :: [((SrcSpan, AnnKeywordId), [SrcSpan])] -> SeldaT Ghc ()
insertTokens ghcTokens = do
  let toInsert = 
        map (\((_, keyw), [sp]) -> 
                 let (file, start_row, start_col, end_row, end_col) = spanData sp
                  in pack file :*: start_row :*: start_col :*: end_row :*: end_col :*: pack (show keyw)
            ) ghcTokens
  insert_ tokens toInsert

insertComments :: [Located AnnotationComment] -> SeldaT Ghc ()
insertComments ghcComments = do
  let toInsert = 
        map (\(L l comm) -> let (file, start_row, start_col, end_row, end_col) = spanData l
                                (kind, text) = categorizeComment comm
                             in pack file :*: start_row :*: start_col :*: end_row :*: end_col :*: pack text :*: pack kind
            ) ghcComments
  insert_ comments toInsert
        
categorizeComment :: AnnotationComment -> (String, String)
categorizeComment (AnnDocCommentNext str) = ("AnnDocCommentNext", str)
categorizeComment (AnnDocCommentPrev str) = ("AnnDocCommentPrev", str)
categorizeComment (AnnDocCommentNamed str) = ("AnnDocCommentNamed", str)
categorizeComment (AnnDocSection _ str) = ("AnnDocSection", str)
categorizeComment (AnnDocOptions str) = ("AnnDocOptions", str)
categorizeComment (AnnLineComment str) = ("AnnLineComment", str)
categorizeComment (AnnBlockComment str) = ("AnnBlockComment", str)

data ExportState = ExportState { parentData :: Maybe (SrcSpan, String) 
                               , exportSyntax :: Bool
                               , compiledModule :: Module
                               }
                               
initExportState :: Bool -> Module -> ExportState
initExportState exportSyntax mod = ExportState Nothing exportSyntax mod

goInto :: SrcSpan -> String -> TrfType a -> TrfType a
goInto sp str = local (\s -> s { parentData = Just (sp, str) })

type TrfType = ReaderT ExportState (SeldaT Ghc)

class HsHasName n => HsName n where
  trfName :: Located n -> TrfType ()
  trfNameOrRdrName :: Located (NameOrRdrName n) -> TrfType ()

instance HsName RdrName where
  trfName (L l _) = writeInsert "Name" "Name" l
  trfNameOrRdrName = trfName

instance HsName Name where
  trfName (L l n) = do writeInsert "Name" "Name" l
                       writeName l n
  trfNameOrRdrName = trfName

trfRnModule :: HsName n => (HsGroup n, [LImportDecl n], Maybe [LIE n], Maybe LHsDocString) -> TrfType ()
trfRnModule (gr,_,_,_) = do
  let binds = case hs_valds gr of ValBindsOut bindGroups _ -> unionManyBags (map snd bindGroups)
  writeInsert "Module" "Module" noSrcSpan
  addToScope (combineLocated $ bagToList binds) (bagToList binds)
  goInto noSrcSpan "mod_decls" $ mapM_ trfBind (bagToList binds)

trfModule :: HsName n => Located (HsModule n) -> TrfType ()
trfModule (L l (HsModule _ _ _ decls _ _)) = do
  writeInsert "Module" "Module" l
  goInto noSrcSpan "mod_decls" $ mapM_ trfDecl decls

trfDecl :: HsName n => Located (HsDecl n) -> TrfType ()
trfDecl (L l (ValD bind)) = do 
  writeInsert "Decl" "Bind" l
  goInto l "decl_bind" $ trfBind (L l bind)

trfBind :: HsName n => Located (HsBind n) -> TrfType ()
trfBind (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _)) = do
  writeInsert "Bind" "Fun" l
  goInto l "bind_matches" $ mapM_ trfMatch matches

trfMatch :: forall n . HsName n => Located (Match n (LHsExpr n)) -> TrfType ()
trfMatch (L l (Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  writeInsert "Match" "Match" l
  goInto l "match_name" $ trfNameOrRdrName @n (mc_fun name)
  goInto l "match_pats" $ mapM_ trfPat pats
  addToScope (combineLocated pats) pats
  goInto l "match_rhss" $ mapM_ trfRhss rhss

trfRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
trfRhss (L l (GRHS [] body)) = do
  writeInsert "Rhs" "Unguarded" l
  goInto l "rhs_body" $ trfExpr body

trfPat :: HsName n => Located (Pat n) -> TrfType ()
trfPat (L l (VarPat name)) = do
  writeInsert "Pattern" "Variable" l
  goInto l "pat_name" $ trfName name

trfExpr :: HsName n => Located (HsExpr n) -> TrfType ()
trfExpr (L l (OpApp e1 (L _ (HsVar op)) _ e2)) = do
  writeInsert "Expr" "InfixApp" l
  goInto l "expr_lhs" $ trfExpr e1
  goInto l "expr_op" $ trfName op
  goInto l "expr_rhs" $ trfExpr e2
trfExpr (L l (HsVar name)) = do
  writeInsert "Expr" "Var" l
  goInto l "expr_name" $ trfName name

----------------------------------------------------------------------

writeInsert :: String -> String -> SrcSpan -> TrfType ()
writeInsert typ ctor loc = do
  expSyntax <- asks exportSyntax
  when expSyntax (writeInsert' typ ctor loc)

writeInsert' :: String -> String -> SrcSpan -> TrfType ()
writeInsert' typ ctor loc = do
  parentRef <- asks parentData
  let ((_, p_st_r, p_st_c, p_nd_r, p_nd_c), p_hndl) 
        = case parentRef of Just (parentSpan, parentHandle) -> (spanData parentSpan, Just parentHandle)
                            Nothing -> (spanData noSrcSpan, Nothing)
      (file, start_row, start_col, end_row, end_col) = spanData loc
  lift $ insert_ nodes [ pack typ :*: pack ctor :*: pack file :*: start_row :*: start_col :*: end_row :*: end_col :*: fmap pack p_hndl ]

writeName :: SrcSpan -> Name -> TrfType ()
writeName loc name = do
  cm <- asks compiledModule 
  let (file, start_row, start_col, end_row, end_col) = spanData loc
      (d_file, d_start_row, d_start_col, d_end_row, d_end_col) = spanData (nameSrcSpan name)
      uniq = maybe ("local." ++ showSDocUnsafe (pprModule cm) ++ "." ++ show (nameUnique name))
                   ((++ ("."++nameStr)) . showSDocUnsafe . pprModule) 
                   (nameModule_maybe name)
      namespace = occNameSpace $ nameOccName name
      nameStr = occNameString $ nameOccName name
  lift $ insert_ names [ pack file :*: start_row :*: start_col :*: end_row :*: end_col
                           :*: Just (pack d_file) :*: Just d_start_row :*: Just d_start_col :*: Just d_end_row :*: Just d_end_col
                           :*: pack (showSDocUnsafe (pprNameSpace namespace)) :*: pack nameStr :*: pack uniq ]

spanData :: SrcSpan -> (String, Int, Int, Int, Int)
spanData sp = case sp of RealSrcSpan rsp -> ( unpackFS (srcSpanFile rsp)
                                            , srcSpanStartLine rsp
                                            , srcSpanStartCol rsp
                                            , srcSpanEndLine rsp
                                            , srcSpanEndCol rsp )
                         _ -> ("", 0, 0, 0, 0)

combineLocated :: [Located a] -> SrcSpan
combineLocated = foldl combineSrcSpans noSrcSpan . map getLoc

-------------------------------------------------------------------------

addToScope :: HsHasName n => SrcSpan -> n -> TrfType ()
addToScope sp named = do
  expSyntax <- asks exportSyntax
  when (not expSyntax) (addToScope' sp named)

addToScope' :: HsHasName n => SrcSpan -> n -> TrfType ()
addToScope' sp named = do
  let (file, start_row, start_col, end_row, end_col) = spanData sp
  newScope <- lift $ insertWithPK scopes [ def :*: pack file :*: start_row :*: start_col :*: end_row :*: end_col ]
  let newNames = map (\n -> let name = pack (occNameString (nameOccName n))
                                namespace = pack (showSDocUnsafe (pprNameSpace $ occNameSpace $ nameOccName n))
                                uniq = pack (show $ nameUnique n)
                             in newScope :*: name :*: namespace :*: uniq
                     ) (hsGetNames named)
  lift $ insert_ scopeNames newNames 

class HsHasName a where
  hsGetNames :: a -> [GHC.Name]

instance HsHasName RdrName where
  hsGetNames _ = []

instance HsHasName Name where
  hsGetNames n = [n]

instance HsHasName e => HsHasName [e] where
  hsGetNames es = concatMap hsGetNames es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames (L _ e) = hsGetNames e

instance HsHasName n => HsHasName (Pat n) where
  hsGetNames (VarPat id) = hsGetNames id

instance HsHasName n => HsHasName (HsBind n) where
  hsGetNames (FunBind {fun_id = lname}) = hsGetNames lname

