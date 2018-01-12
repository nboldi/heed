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
import Outputable hiding (text)
import FastString
import GHC.Paths ( libdir )
import DynFlags
import UniqDFM
import Module
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
    do sess <- lift getSession
       eps <- liftIO $ hscEPS sess
       let pit = eps_PIT eps
           hpt = hsc_HPT sess
       mapM_ (\m -> writeImportedNames (mi_module $ hm_iface m) (concatMap (\a -> map (\n -> if n == availName a then (n, Nothing) else (n, Just $ availName a)) $ availNamesWithSelectors a) $ md_exports $ hm_details m)) (eltsUDFM hpt)
       mapM_ (\m -> writeImportedNames (mi_module m) (concatMap (\a -> map (\n -> if n == availName a then (n, Nothing) else (n, Just $ availName a)) $ availNames a) $ mi_exports m)) (moduleEnvElts pit)
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
