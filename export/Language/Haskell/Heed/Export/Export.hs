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
  df <- liftGhc getSessionDynFlags
  initDatabase
  p <- liftGhc $ parseModule ms
  let (ghcTokens, ghcComments) = pm_annotations $ p
      tokenKeys = Map.assocs ghcTokens
  t <- liftGhc $ typecheckModule p
  transaction $ do
    liftIO $ logInfo df (defaultUserStyle df) (ppr (ms_mod ms) Outputable.<> Outputable.text ": transaction start")
    im <- lookupImportedModule (pack $ moduleNameString $ moduleName (ms_mod ms)) (pack $ show $ moduleUnitId (ms_mod ms))
    modId <- case im of []   -> do liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "module is not in the database yet, registering")
                                   df <- liftGhc getSessionDynFlags
                                   res <- writeModule ms
                                   return res
                        mods -> do liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "module is in the database, cleaning")
                                   withForeignCheckTurnedOff $ removeModuleFromDB ms (map first mods) --check deactivated for performance
                                   return (first $ head mods)
    liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "module created")
    do sess <- lift getSession
       eps <- liftIO $ hscEPS sess
       let pit = eps_PIT eps
           hpt = hsc_HPT sess
       mapM_ (\m -> updateImported availNamesWithSelectors (hm_iface m) (md_exports $ hm_details m)) (eltsUDFM hpt)
       mapM_ (\m -> updateImported availNames m (mi_exports m)) (moduleEnvElts pit)
       liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "accessible names loaded")
    insertTokens tokenKeys
    insertComments (concat $ Map.elems ghcComments)
    liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "tokens and comments inserted")
    eof <- query $ do tok <- select tokens
                      restrict $ tok ! token_str .== text (pack (show AnnEofPos))
                      return tok
    let moduleLoc = case eof of [ file :*: _ :*: _ :*: er :*: ec :*: _ ]
                                  -> mkSrcSpan (mkSrcLoc (mkFastString (unpack file)) 1 1)
                                               (mkSrcLoc (mkFastString (unpack file)) er ec)
                                _ -> noSrcSpan
    let gblEnv = fst $ tm_internals_ t
        exportSt = initExportState ms moduleLoc
    df <- liftGhc getSessionDynFlags
    store1 <- execWriterT $ flip runReaderT (exportSt emptyStore modId ParsedStage (Just gblEnv)) $ exportModule $ parsedSource p
    liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "parsed stage done")
    store2 <- execWriterT $ case renamedSource t of Just rs -> flip runReaderT (exportSt store1 modId RenameStage Nothing) $ exportRnModule rs
    liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "renamed stage done")
    runWriterT $ flip runReaderT (exportSt store2 modId TypedStage Nothing) $ exportTcModule $ typecheckedSource t
    liftIO $ logInfo df (defaultUserStyle df) (Outputable.text $ "typechecked stage done")
    return ()

updateImported :: (AvailInfo -> [GHC.Name]) -> ModIface -> [AvailInfo] -> SeldaT Ghc ()
updateImported f m avails
  = do let hash = pack $ show (mi_iface_hash m)
           mod = mi_module m
       modIds <- lookupImportedModule (pack $ moduleNameString $ moduleName (mi_module m)) (pack $ show $ moduleUnitId (mi_module m))
       (mId :*: hash')
         <- if (null modIds)
              then do (:*: Nothing) <$> insertWithPK modules [ def :*: pack (moduleNameString $ moduleName mod)
                                                         :*: pack (show $ moduleUnitId mod)
                                                         :*: Nothing
                                                         :*: Just hash ]
              else return $ head modIds
       when (Just hash /= hash') $ do
         writeImportedNames (mi_module m) mId (concatMap (\a -> map (availToPair a) $ f a) avails)
         deleteFrom_ definitions (\d -> d ! def_module .== just (literal mId))
         update_ modules (\m -> m ! module_id .== literal mId) (\m -> m `with` [module_hash := just (text hash)])
  where availToPair a n = if n == availName a then (n, Nothing) else (n, Just $ availName a)

removeModuleFromDB :: ModSummary -> [RowID] -> SeldaT Ghc ()
removeModuleFromDB ms (map literal -> modIDs) = do
  df <- liftGhc getSessionDynFlags
  let fileName = text (pack (fromJust $ ml_hs_file $ ms_location ms))
      isFromModule n = n ! node_file .== fileName
  deleteFrom_ names (\name -> name ! name_module `isIn` modIDs)
  deleteFrom_ attributes (\attr -> attr ! attribute_module `isIn` modIDs)
  deleteFrom_ definitions (\d -> d ! def_module `isIn` map just modIDs)
  deleteFrom_ types (\t -> t ! type_module `isIn` modIDs)
  deleteFrom_ implicitBinds (\imp -> imp ! imp_bind_module `isIn` modIDs)
  deleteFrom_ ctorFields (\cf -> cf ! cf_module `isIn` modIDs)
  deleteFrom_ typeCtors (\ct -> ct ! ct_module `isIn` modIDs)
  deleteFrom_ moduleImports (\mi -> mi ! mi_contain_module `isIn` modIDs)
  deleteFrom_ moduleImportShowing (\mis -> mis ! mis_module `isIn` modIDs)
  deleteFrom_ moduleImportHiding (\mih -> mih ! mih_module `isIn` modIDs)

  deleteFrom_ nodes (\sc -> sc ! node_module `isIn` modIDs)
  deleteFrom_ scopes (\sc -> sc ! scope_file .== fileName)
  deleteFrom_ tokens (\t -> t ! token_file .== fileName)
  deleteFrom_ comments (\c -> c ! comment_file .== fileName)
  -- deleteFrom_ modules (\m -> m ! module_id `isIn` modIDs) -- modules are not deleted

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
