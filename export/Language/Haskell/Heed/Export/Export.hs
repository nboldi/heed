{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Heed.Export.Export where

import Control.Monad.Reader
import Control.Monad.Writer
import Database.Selda
import Database.Selda.SQLite
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as Map
import Data.Text (pack, unpack)

import Avail
import HscTypes
import GHC
import Fingerprint
import qualified Outputable
import Outputable hiding (text, int)
import FastString
import GHC.Paths ( libdir )
import DynFlags
import UniqDFM
import Module
import ErrUtils
import SrcLoc
import TcRnTypes

import Language.Haskell.Heed.Export.Modules
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Lexical

exportSrcFile :: FilePath -> FilePath -> String -> Bool -> IO ()
exportSrcFile dbPath root modName doExport =
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags
      $ flip gopt_set Opt_KeepRawTokenStream
      $ dflags { importPaths = [root] ++ importPaths dflags }
    target <- guessTarget modName Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName modName
    when doExport $ do
      -- withSQLite dbPath $ cleanDatabase
      exportModSummary dbPath modSum

exportModSummary :: FilePath -> ModSummary -> Ghc ()
exportModSummary db ms = do
  df <- getSessionDynFlags
  p <- parseModule ms
  let (ghcTokens, ghcComments) = pm_annotations $ p
      tokenKeys = Map.assocs ghcTokens
  t <- typecheckModule p
  let tc = fst $ tm_internals_ t
      hpm = HsParsedModule (pm_parsed_source p) (pm_extra_src_files p) (pm_annotations p)
      modRecord = ( ms , hpm , tc )
  exportModStuff db modRecord
  exportGlobalStuff db

-- parsedAction :: [String] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
-- parsedAction [db] ms pm = withSQLite db $ transaction $ do
--   let (ghcTokens, ghcComments) = hpm_annotations $ p
--       tokenKeys = Map.assocs ghcTokens
--   insertTokens tokenKeys
--   insertComments (concat $ Map.elems ghcComments)
--   eof <- query $ do tok <- select tokens
--                     restrict $ tok ! token_str .== text (pack (show AnnEofPos))
--                     return tok
--   let moduleLoc = case eof of [ file :*: _ :*: _ :*: er :*: ec :*: _ ]
--                                 -> mkSrcSpan (mkSrcLoc (mkFastString (unpack file)) 1 1)
--                                              (mkSrcLoc (mkFastString (unpack file)) er ec)
--                               _ -> noSrcSpan
--   let gblEnv = fst $ tm_internals_ t
--       exportSt = initExportState ms moduleLoc
--   df <- liftGhc getSessionDynFlags
--   execWriterT $ flip runReaderT (exportSt emptyStore modId ParsedStage (Just gblEnv)) $ exportModule $ parsedSource p
--
-- typecheckedAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> Hsc HsParsedModule
-- typecheckedAction [db] ms tc = withSQLite db $ transaction $ do
--   evaluatedNodes <- getEvaluatedNodes modId
--   let store1 = ExportStore [] evaluatedNodes
--   execWriterT $ case renamedSource t of Just rs -> flip runReaderT (exportSt store1 modId RenameStage Nothing) $ exportRnModule rs
--   ambiguousNames <- getAmbiguousNames modId
--   let store2 = ExportStore ambiguousNames evaluatedNodes
--   runWriterT $ flip runReaderT (exportSt store2 modId TypedStage Nothing) $ exportTcModule $ typecheckedSource t
--
-- spliceRunAction :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
-- spliceRunAction [db] tc = liftIO $ withSQLite db $ transaction $ exportExpression tc
--
-- interfaceLoadAction :: forall lcl . [CommandLineOption] -> ModIface -> IfM lcl ModIface
-- interfaceLoadAction [db] m
--   = liftIO $ withSQLite db $ transaction $ updateImported availNames m (mi_exports m)

type ModRecord = (ModSummary, HsParsedModule, TcGblEnv)

exportModStuff :: FilePath -> ModRecord -> Ghc ()
exportModStuff db (ms, p, t) = do
  df <- getSessionDynFlags
  let (ghcTokens, ghcComments) = hpm_annotations p
      tokenKeys = Map.assocs ghcTokens
      fileName = case getLoc $ hpm_module p of
                   RealSrcSpan sp -> unpackFS $ srcSpanFile sp
                   _              -> "<nofile>"
  withExportHandles $ \handles -> do
    -- liftIO $ logInfo df (defaultUserStyle df) (ppr (ms_mod ms) Outputable.<> Outputable.text ": transaction start")
    -- liftIO $ logInfo df (defaultUserStyle df) (ppr (ms_mod ms) Outputable.<> Outputable.text ": database initialized")
    -- TODO: make sure that the module exists in the DB
    -- liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "module created")
    insertTokens tokenKeys
    insertComments (concat $ Map.elems ghcComments)
    -- liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "tokens and comments inserted")
    let moduleLoc = case find (\((_,kw),_) -> kw == AnnEofPos) tokenKeys of
                      Just (_, [l@(RealSrcSpan sp)]) -> mkSrcSpan (mkSrcLoc (srcSpanFile sp) 1 1) (srcSpanEnd l)
                      _ -> noSrcSpan
    let exportSt = initExportState ms moduleLoc fileName handles
    df <- getSessionDynFlags
    flip runReaderT (exportSt emptyStore ParsedStage (Just t)) $ exportModule $ hpm_module p
    -- liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "parsed stage done")
    evaluatedNodes <- return [] -- getEvaluatedNodes
    let store1 = ExportStore [] evaluatedNodes
    case getRenamed t of
      Just rs -> flip runReaderT (exportSt store1 RenameStage Nothing) $ exportRnModule rs
      Nothing -> error "Renamed stuff is not found"
    ambiguousNames <- return [] -- getAmbiguousNames modId
    let store2 = ExportStore ambiguousNames evaluatedNodes
    -- liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "renamed stage done")
    flip runReaderT (exportSt store2 TypedStage Nothing) $ exportTcModule $ (tcg_binds t)
    -- liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "typechecked stage done")
    return ()

exportGlobalStuff :: FilePath -> Ghc ()
exportGlobalStuff db = do
  sess <- getSession
  eps <- liftIO $ hscEPS sess
  let pit = eps_PIT eps
      hpt = hsc_HPT sess
  -- liftIO $ logInfo df (defaultUserStyle df) $ Outputable.text "accessible interfaces: " Outputable.<> ppr ( map (mi_module . hm_iface) (eltsUDFM hpt) ++ map mi_module (moduleEnvElts pit) )
  mapM_ (\m -> updateImported availNames m (mi_exports m)) (moduleEnvElts pit)
  -- liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "accessible names loaded")

type RenamedStuff =
       (HsGroup GHC.Name, [LImportDecl GHC.Name], Maybe [LIE GHC.Name], Maybe LHsDocString)

getRenamed :: TcGblEnv -> Maybe RenamedStuff
getRenamed tc_result = case tcg_rn_decls tc_result of
                         Just decl -> Just ( decl, tcg_rn_imports tc_result
                                                 , tcg_rn_exports tc_result
                                                 , tcg_doc_hdr tc_result )
                         Nothing -> Nothing

updateImported :: (AvailInfo -> [GHC.Name]) -> ModIface -> [AvailInfo] -> Ghc ()
updateImported f m avails
  = return () -- TODO

 -- do let hash = pack $ show (mi_iface_hash m)
 --           mod = mi_module m
 --       modIds <- lookupImportedModule (pack $ moduleNameString $ moduleName (mi_module m)) (pack $ show $ moduleUnitId (mi_module m))
 --       (mId :*: hash')
 --         <- if (null modIds)
 --              then do (:*: Nothing) <$> insertWithPK modules [ def :*: pack (moduleNameString $ moduleName mod)
 --                                                         :*: pack (show $ moduleUnitId mod)
 --                                                         :*: Nothing
 --                                                         :*: Just hash ]
 --              else return $ head modIds
 --       df <- liftGhc getSessionDynFlags
 --       when (Just hash /= hash') $ do
 --         deleteFrom_ definitions (\d -> d ! def_module .== just (literal mId))
 --         writeImportedNames (mi_module m) mId (concatMap (\a -> map (availToPair a) $ f a) avails)
 --         update_ modules (\m -> m ! module_id .== literal mId)
 --                 (\m -> m `with` [module_hash := just (text hash)])
 --  where availToPair a n = if n == availName a then (n, Nothing) else (n, Just $ availName a)

removeModuleFromDB :: ModSummary -> SeldaT Ghc ()
removeModuleFromDB ms = return ()
  -- TODO
