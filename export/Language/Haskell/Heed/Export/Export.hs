{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Heed.Export.Export where

import Control.Monad.Reader
import Control.Monad.Writer
import Database.Selda
import Database.Selda.SQLite
import Data.List
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

import Core
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
      withSQLite dbPath $ cleanDatabase
      exportModSummary dbPath modSum

exportModSummary :: FilePath -> ModSummary -> Ghc ()
exportModSummary db ms = withSQLite db $ do
  initDatabase
  p <- liftGhc $ parseModule ms
  let (ghcTokens, ghcComments) = pm_annotations $ p
      tokenKeys = Map.assocs ghcTokens
  t <- liftGhc $ typecheckModule p
  transaction $ do
    removeModuleFromDB ms
    do sess <- lift getSession
       eps <- liftIO $ hscEPS sess
       let pit = eps_PIT eps
           hpt = hsc_HPT sess
       mapM_ (\m -> updateImported availNamesWithSelectors (hm_iface m) (md_exports $ hm_details m)) (eltsUDFM hpt)
       mapM_ (\m -> updateImported availNames m (mi_exports m)) (moduleEnvElts pit)
    insertTokens tokenKeys
    insertComments (concat $ Map.elems ghcComments)
    eof <- query $ do tok <- select tokens
                      restrict $ tok ! token_str .== text (pack (show AnnEofPos))
                      return tok
    let moduleLoc = case eof of [ file :*: _ :*: _ :*: er :*: ec :*: _ ]
                                  -> mkSrcSpan (mkSrcLoc (mkFastString (unpack file)) 1 1)
                                               (mkSrcLoc (mkFastString (unpack file)) er ec)
                                _ -> noSrcSpan
    let gblEnv = fst $ tm_internals_ t
        exportSt = initExportState ms moduleLoc
    store1 <- execWriterT $ flip runReaderT (exportSt emptyStore ParsedStage (Just gblEnv)) $ exportModule $ parsedSource p
    store2 <- execWriterT $ case renamedSource t of Just rs -> flip runReaderT (exportSt store1 RenameStage Nothing) $ exportRnModule rs
    runWriterT $ flip runReaderT (exportSt store2 TypedStage Nothing) $ exportTcModule $ typecheckedSource t
    return ()

updateImported :: (AvailInfo -> [GHC.Name]) -> ModIface -> [AvailInfo] -> SeldaT Ghc ()
updateImported f m avails
  = do let hash = pack $ show (mi_iface_hash m)
           mod = mi_module m
       modIds <- lookupImportedModule (pack $ moduleNameString $ moduleName (mi_module m)) (pack $ show $ moduleUnitId (mi_module m))
       (mId :*: hash')
         <- if (null modIds)
              then (:*: Nothing) <$> insertWithPK modules [ def :*: pack (moduleNameString $ moduleName mod)
                                                         :*: pack (show $ moduleUnitId mod)
                                                         :*: Nothing
                                                         :*: Just hash ]
              else return $ head modIds
       when (Just hash /= hash') $ do
         writeImportedNames (mi_module m) (concatMap (\a -> map (availToPair a) $ f a) avails)
         update_ modules (\m -> m ! module_id .== literal mId) (\m -> m `with` [module_hash := just (text hash)])
  where availToPair a n = if n == availName a then (n, Nothing) else (n, Just $ availName a)

removeModuleFromDB :: ModSummary -> SeldaT Ghc ()
removeModuleFromDB ms = do
  let fileName = text (pack (fromJust $ ml_hs_file $ ms_location ms))
      isFromModule n = n ! node_file .== fileName
  nodesToDelete <- query $ do node <- select nodes
                              restrict $ isFromModule node
                              return (node ! node_id)
  modulesToDelete <- query $ do m <- select modules
                                restrict $ m ! module_source .== just fileName
                                return (m ! module_id)
  scopesToDelete <- query $ do sc <- select scopes
                               restrict $ sc ! scope_file .== fileName
                               return (sc ! scope_id)
  namesToDelete <- query $ do node <- select nodes
                              name <- select names
                              restrict $ isFromModule node
                              restrict $ name ! name_node `isIn` map literal nodesToDelete
                              return (name ! name_uniq)
  deleteFrom_ names (\name -> name ! name_node `isIn` map literal nodesToDelete)
  deleteFrom_ attributes (\attr -> attr ! container `isIn` map literal nodesToDelete)
  deleteFrom_ definitions (\d -> d ! def_module `isIn` map (just . literal) modulesToDelete
                                  .|| d ! def_scope `isIn` map (just . literal) scopesToDelete)
  deleteFrom_ nodes isFromModule
  deleteFrom_ modules (\m -> m ! module_source .== just (text $ pack $ fromJust $ ml_hs_file $ ms_location ms))
  deleteFrom_ types (\t -> t ! type_name `isIn` map text namesToDelete)
  deleteFrom_ implicitBinds (\imp -> imp ! imp_bind_lhs `isIn` map text namesToDelete .|| imp ! imp_bind_rhs `isIn` map text namesToDelete)
  deleteFrom_ ctorFields (\cf -> cf ! cf_constructor `isIn` map text namesToDelete .|| cf ! cf_field `isIn` map text namesToDelete)
  deleteFrom_ typeCtors (\ct -> ct ! ct_type `isIn` map text namesToDelete .|| ct ! ct_ctor `isIn` map text namesToDelete)
  deleteFrom_ moduleImports (\mi -> mi ! mi_scope_id `isIn` map literal scopesToDelete .|| mi ! mi_module_id `isIn` map literal modulesToDelete)
  deleteFrom_ moduleImportShowing (\mis -> mis ! mis_node `isIn` map literal nodesToDelete)
  deleteFrom_ moduleImportHiding (\mih -> mih ! mih_node `isIn` map literal nodesToDelete)

  deleteFrom_ tokens (\t -> t ! token_file .== fileName)
  deleteFrom_ comments (\c -> c ! comment_file .== fileName)

initDatabase :: SeldaT Ghc ()
initDatabase = do
  tryCreateTable tokens
  tryCreateTable comments
  tryCreateTable nodes
  tryCreateTable names
  tryCreateTable definitions
  tryCreateTable scopes
  tryCreateTable attributes
  tryCreateTable types
  tryCreateTable implicitBinds
  tryCreateTable modules
  tryCreateTable ctorFields
  tryCreateTable typeCtors
  tryCreateTable moduleImports
  tryCreateTable moduleImportShowing
  tryCreateTable moduleImportHiding

cleanDatabase :: SeldaT Ghc ()
cleanDatabase = withForeignCheckTurnedOff $ do
   tryDropTable tokens
   tryDropTable comments
   tryDropTable nodes
   tryDropTable names
   tryDropTable definitions
   tryDropTable scopes
   tryDropTable attributes
   tryDropTable types
   tryDropTable implicitBinds
   tryDropTable modules
   tryDropTable ctorFields
   tryDropTable typeCtors
   tryDropTable moduleImports
   tryDropTable moduleImportShowing
   tryDropTable moduleImportHiding
