{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Main where

import Control.Monad.State
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe
import qualified Data.Map as Map

import GHC
import Outputable
import SrcLoc
import Bag
import FastString
 
import GHC.Paths ( libdir )
import DynFlags
import Name
import OccName
 
main :: IO ()
main = example

fileName = "A.hs"
modName = "A"

example = 
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        void $ setSessionDynFlags
          $ flip gopt_set Opt_KeepRawTokenStream dflags
        target <- guessTarget fileName Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName modName
        p <- parseModule modSum
        let (tokens, comments) = pm_annotations $ p
            tokenKeys = Map.assocs tokens
        t <- typecheckModule p
        d <- desugarModule t
        
        conn <- liftIO $ connectSqlite3 "..\\haskell.db"
        liftIO $ run conn "DELETE FROM nodes;" []
        liftIO $ run conn "DELETE FROM name_infos;" []
        liftIO $ run conn "DELETE FROM scopes;" []
        liftIO $ run conn "DELETE FROM scope_names;" []
        liftIO $ run conn "DELETE FROM tokens;" []
        liftIO $ run conn "DELETE FROM comments;" []
        st <- liftIO $ createExportState conn
        
        liftIO $ insertTokens conn tokenKeys
        liftIO $ insertComments conn (concat $ Map.elems comments)
        liftIO $ flip runReaderT (st { exportSyntax = True }) $ trfModule $ parsedSource d
        case renamedSource d of Just rs -> liftIO $ flip runReaderT st $ trfRnModule rs
        liftIO $ commit conn
        liftIO $ disconnect conn

insertTokens :: Connection -> [((SrcSpan, AnnKeywordId), [SrcSpan])] -> IO ()
insertTokens conn tokens = do
  stmt <- prepare conn "INSERT INTO tokens (file, start_row, start_col, end_row, end_col, token) VALUES ( ?, ?, ?, ?, ?, ? )"
  mapM_ (\((_, keyw), [sp]) -> do let (file, start_row, start_col, end_row, end_col) = spanData sp
                                  execute stmt [ toSql file, toSql start_row, toSql start_col, toSql end_row
                                               , toSql end_col, toSql (show keyw) ]) tokens

insertComments :: Connection -> [Located AnnotationComment] -> IO ()
insertComments conn comments = do
  stmt <- prepare conn "INSERT INTO comments (content, type, file, start_row, start_col, end_row, end_col) VALUES ( ?, ?, ?, ?, ?, ?, ? )"
  mapM_ (\(L l comm) -> do let (file, start_row, start_col, end_row, end_col) = spanData l
                               (kind, text) = case comm of AnnDocCommentNext str -> ("AnnDocCommentNext", str)
                                                           AnnDocCommentPrev str -> ("AnnDocCommentPrev", str)
                                                           AnnDocCommentNamed str -> ("AnnDocCommentNamed", str)
                                                           AnnDocSection _ str -> ("AnnDocSection", str)
                                                           AnnDocOptions str -> ("AnnDocOptions", str)
                                                           AnnLineComment str -> ("AnnLineComment", str)
                                                           AnnBlockComment str -> ("AnnBlockComment", str)
                           execute stmt [ toSql text, toSql kind, toSql file, toSql start_row, toSql start_col
                                        , toSql end_row, toSql end_col ]
        ) comments


data ExportState = ExportState { parentData :: Maybe (SrcSpan, String) 
                               , nodeInsert :: Statement
                               , nameInsert :: Statement
                               , scopeInsert :: Statement
                               , scopeExtend :: Statement
                               , getLastId :: Statement
                               , exportSyntax :: Bool
                               }

createExportState :: Connection -> IO ExportState
createExportState conn = do
  nodeIns <- prepare conn "INSERT INTO nodes (type, ctor, file, start_row, start_col, end_row, end_col, parent_start_row, parent_start_col, parent_end_row, parent_end_col, parent_handle) VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
  nameIns <- prepare conn "INSERT INTO name_infos (file, start_row, start_col, end_row, end_col, def_file, def_start_row, def_start_col, def_end_row, def_end_col, name, uniq, namespace) VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? );"
  scopeIns <- prepare conn "INSERT INTO scopes (file, start_row, start_col, end_row, end_col) VALUES ( ?, ?, ?, ?, ? );"
  scopeExtend <- prepare conn "INSERT INTO scope_names (scope_id, name, namespace, uniq) VALUES ( ?, ?, ?, ? );"
  getLastId <- prepare conn "SELECT last_insert_rowid()"
  return $ ExportState Nothing nodeIns nameIns scopeIns scopeExtend getLastId False

goInto :: SrcSpan -> String -> TrfType a -> TrfType a
goInto sp str = local (\s -> s { parentData = Just (sp, str) })

type TrfType = ReaderT ExportState IO

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
        = case parentRef of Just (parentSpan, parentHandle) -> (spanData parentSpan, parentHandle)
                            Nothing -> (spanData noSrcSpan, "")
      (file, start_row, start_col, end_row, end_col) = spanData loc
  stmt <- asks nodeInsert
  void $ liftIO $ execute stmt [ toSql typ, toSql ctor, toSql file, toSql start_row, toSql start_col
                               , toSql end_row, toSql end_col, toSql p_st_r, toSql p_st_c, toSql p_nd_r
                               , toSql end_col, toSql p_hndl ]

writeName :: SrcSpan -> Name -> TrfType ()
writeName loc name = do
  let (file, start_row, start_col, end_row, end_col) = spanData loc
      (d_file, d_start_row, d_start_col, d_end_row, d_end_col) = spanData (nameSrcSpan name)
      uniq = nameUnique name
      namespace = occNameSpace $ nameOccName name
      nameStr = occNameString $ nameOccName name
  stmt <- asks nameInsert
  void $ liftIO $ execute stmt [ toSql file, toSql start_row, toSql start_col, toSql end_row, toSql end_col
                               , toSql d_file, toSql d_start_row, toSql d_start_col, toSql d_end_row, toSql d_end_col
                               , toSql nameStr, toSql (show uniq), toSql (showSDocUnsafe $ pprNameSpace namespace) ]

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
  createStmt <- asks scopeInsert
  extendStmt <- asks scopeExtend
  void $ liftIO $ execute createStmt [ toSql file, toSql start_row, toSql start_col, toSql end_row, toSql end_col ]
  newScope <- askLastId
  mapM_ (\n -> liftIO $ execute extendStmt [ newScope
                                           , toSql (occNameString $ nameOccName n)
                                           , toSql (showSDocUnsafe $ pprNameSpace $ occNameSpace $ nameOccName n)
                                           , toSql (show $ nameUnique n) 
                                           ])
        (hsGetNames named)

askLastId :: TrfType SqlValue
askLastId = do lastId <- asks getLastId
               liftIO $ execute lastId []
               liftIO $ head . fromMaybe (error "askLastId: no last id") <$> fetchRow lastId

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

