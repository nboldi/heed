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

exportSrcFile :: FilePath -> String -> Bool -> IO ()
exportSrcFile root modName doExport =
  runGhc (Just libdir) $
    withSQLite "haskell.db" $ do
      dflags <- liftGhc getSessionDynFlags
      void $ liftGhc $ setSessionDynFlags
        $ flip gopt_set Opt_KeepRawTokenStream
        $ dflags { importPaths = [root] ++ importPaths dflags }
      target <- liftGhc $ guessTarget modName Nothing
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
          do sess <- lift getSession
             eps <- liftIO $ hscEPS sess
             let pit = eps_PIT eps
                 hpt = hsc_HPT sess
             mapM_ (\m -> writeImportedNames (mi_module $ hm_iface m) (concatMap availNames $ md_exports $ hm_details m)) (eltsUDFM hpt)
             mapM_ (\m -> writeImportedNames (mi_module m) (concatMap availNames $ mi_exports m)) (moduleEnvElts pit)
          insertTokens tokenKeys
          insertComments (concat $ Map.elems ghcComments)
          eof <- query $ do tok <- select tokens
                            restrict $ tok ! token_str .== text (pack (show AnnEofPos))
                            return tok
          let moduleLoc = case eof of [ file :*: _ :*: _ :*: er :*: ec :*: _ ]
                                        -> mkSrcSpan (mkSrcLoc (mkFastString (unpack file)) 1 1)
                                                     (mkSrcLoc (mkFastString (unpack file)) er ec)
                                      _ -> noSrcSpan
          runWriterT $ flip runReaderT (initExportState ParsedStage modSum emptyStore moduleLoc) $ exportModule $ parsedSource p
          store <- execWriterT $ case renamedSource t of Just rs -> flip runReaderT (initExportState RenameStage modSum emptyStore moduleLoc) $ exportRnModule rs
          runWriterT $ flip runReaderT (initExportState TypedStage modSum store moduleLoc) $ exportTcModule $ typecheckedSource t
          return ()

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

   tryDropTable attributes
   createTable attributes

   tryDropTable types
   createTable types

   tryDropTable implicitBinds
   createTable implicitBinds

   tryDropTable modules
   createTable modules

   tryDropTable ctorFields
   createTable ctorFields

   tryDropTable typeCtors
   createTable typeCtors

   tryDropTable moduleImports
   createTable moduleImports
