{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, TypeSynonymInstances, FlexibleInstances, BangPatterns, OverloadedStrings #-}
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
import Outputable (showSDocUnsafe, ppr)
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
    withSQLite "haskell.db" $ do
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
        transaction $ do
          insertTokens tokenKeys
          insertComments (concat $ Map.elems ghcComments)
          let es = initExportState True (ms_mod modSum)
          flip runReaderT es $ trfModule $ parsedSource p
          case renamedSource t of Just rs -> flip runReaderT (es {exportSyntax = False}) $ trfRnModule rs

cleanDatabase :: SeldaT Ghc ()
cleanDatabase = withForeignCheckTurnedOff $ do
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

data ExportState = ExportState { parentData :: Maybe (RowID, String) 
                               , exportSyntax :: Bool
                               , compiledModule :: Module
                               , isDefining :: Bool
                               , scope :: RowID
                               }
                               
initExportState :: Bool -> Module -> ExportState
initExportState exportSyntax mod = ExportState Nothing exportSyntax mod False undefined

type TrfType = ReaderT ExportState (SeldaT Ghc)

class HsHasName n => HsName n where
  trfName :: Located n -> TrfType ()
  trfNameOrRdrName :: Located (NameOrRdrName n) -> TrfType ()

instance HsName RdrName where
  trfName (L l _) = void $ writeInsert "Name" "Name" l
  trfNameOrRdrName = trfName

instance HsName Name where
  trfName (L l n) = writeName l n
  trfNameOrRdrName = trfName

trfRnModule :: HsName n => (HsGroup n, [LImportDecl n], Maybe [LIE n], Maybe LHsDocString) -> TrfType ()
trfRnModule (gr,_,_,_) = do
  let binds = case hs_valds gr of ValBindsOut bindGroups _ -> unionManyBags (map snd bindGroups)
  id <- writeInsert "Module" "Module" noSrcSpan
  addToScope (combineLocated $ bagToList binds)
    $ goInto id "mod_decls" $ mapM_ trfBind (bagToList binds)

trfModule :: HsName n => Located (HsModule n) -> TrfType ()
trfModule (L l (HsModule _ _ _ decls _ _)) = do
  id <- writeInsert "Module" "Module" l
  goInto id "mod_decls" $ mapM_ trfDecl decls

trfDecl :: HsName n => Located (HsDecl n) -> TrfType ()
trfDecl (L l (ValD bind)) = do 
  id <- writeInsert "Decl" "Bind" l
  goInto id "decl_bind" $ trfBind (L l bind)

trfBind :: HsName n => Located (HsBind n) -> TrfType ()
trfBind (L l (FunBind name (MG (L _ matches) _ _ _) _ _ _)) = do
  id <- writeInsert "Bind" "Fun" l
  goInto id "bind_matches" $ mapM_ trfMatch matches

trfMatch :: forall n . HsName n => Located (Match n (LHsExpr n)) -> TrfType ()
trfMatch (L l (Match name pats _ (GRHSs rhss (L _ locBinds)))) = do
  id <- writeInsert "Match" "Match" l
  defining $ goInto id "match_name" $ trfNameOrRdrName @n (mc_fun name)
  addToScope (combineLocated pats) $ do
   goInto id "match_pats" $ mapM_ trfPat pats
   goInto id "match_rhss" $ mapM_ trfRhss rhss

trfRhss :: HsName n => Located (GRHS n (LHsExpr n)) -> TrfType ()
trfRhss (L l (GRHS [] body)) = do
  id <- writeInsert "Rhs" "Unguarded" l
  goInto id "rhs_body" $ trfExpr body

trfPat :: HsName n => Located (Pat n) -> TrfType ()
trfPat (L l (VarPat name)) = do
  id <- writeInsert "Pattern" "Variable" l
  defining $ goInto id "pat_name" $ trfName name

trfExpr :: HsName n => Located (HsExpr n) -> TrfType ()
trfExpr (L l (OpApp e1 (L _ (HsVar op)) _ e2)) = do
  id <- writeInsert "Expr" "InfixApp" l
  goInto id "expr_lhs" $ trfExpr e1
  goInto id "expr_op" $ trfName op
  goInto id "expr_rhs" $ trfExpr e2
trfExpr (L l (HsVar name)) = do
  id <- writeInsert "Expr" "Var" l
  goInto id "expr_name" $ trfName name

----------------------------------------------------------------------

goInto :: Maybe RowID -> String -> TrfType a -> TrfType a
goInto (Just id) str = local (\s -> s { parentData = Just (id, str) })
goInto Nothing _ = id

defining :: TrfType a -> TrfType a
defining = local $ \s -> s { isDefining = True }

writeInsert :: String -> String -> SrcSpan -> TrfType (Maybe RowID)
writeInsert typ ctor loc = do
  expSyntax <- asks exportSyntax
  if expSyntax then Just <$> writeInsert' typ ctor loc
               else return Nothing

writeInsert' :: String -> String -> SrcSpan -> TrfType RowID
writeInsert' typ ctor loc = do
  parentRef <- asks parentData
  let (file, start_row, start_col, end_row, end_col) = spanData loc
  lift $ insertWithPK nodes [ def :*: fmap fst parentRef :*: pack typ :*: pack ctor :*: pack file 
                                :*: start_row :*: start_col :*: end_row :*: end_col 
                                :*: fmap (pack . snd) parentRef ]

lookupNameNode :: Text -> Int -> Int -> SeldaT Ghc [RowID]
lookupNameNode = prepared $ \file start_row start_col -> do 
  n <- select nodes
  restrict $ file .== n ! node_file 
              .&& start_row .== n ! node_start_row
              .&& start_col .== n ! node_start_col
              .&& text "Name" .== n ! node_type
  return (n ! node_id)

writeName :: SrcSpan -> Name -> TrfType ()
writeName sp name = do
  cm <- asks compiledModule 
  sc <- asks scope 
  defining <- asks isDefining 
  let (file, start_row, start_col, _, _) = spanData sp
  [nodeId] <- lift $ lookupNameNode (pack file) start_row start_col
    
  let (d_file, d_start_row, d_start_col, d_end_row, d_end_col) 
        = case nameSrcSpan name of RealSrcSpan rsp -> ( Just (unpackFS (srcSpanFile rsp)), Just (srcSpanStartLine rsp)
                                                      , Just (srcSpanStartCol rsp), Just (srcSpanEndLine rsp)
                                                      , Just (srcSpanEndCol rsp) )
                                   _               -> (Nothing, Nothing, Nothing, Nothing, Nothing)
                               
      uniq = maybe ("local." ++ showSDocUnsafe (pprModule cm) ++ "." ++ show (nameUnique name))
                   ((++ ("."++nameStr)) . showSDocUnsafe . pprModule) 
                   (nameModule_maybe name)
      namespace = occNameSpace $ nameOccName name
      nameStr = occNameString $ nameOccName name
  lift $ insert_ names [ Just nodeId :*: sc :*: fmap pack d_file :*: d_start_row :*: d_start_col :*: d_end_row :*: d_end_col
                           :*: pack (showSDocUnsafe (pprNameSpace namespace)) :*: pack nameStr :*: pack uniq :*: defining ]

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

addToScope :: SrcSpan -> TrfType () -> TrfType ()
addToScope sp act = do
  expSyntax <- asks exportSyntax
  if expSyntax then act else doAddToScope sp act

doAddToScope :: SrcSpan -> TrfType () -> TrfType ()
doAddToScope sp act = do
  let (file, start_row, start_col, end_row, end_col) = spanData sp
  newScope <- lift $ insertWithPK scopes [ def :*: pack file :*: start_row :*: start_col :*: end_row :*: end_col ]
  local (\s -> s { scope = newScope}) act

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

