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
import Data.Text (pack)

import GHC
import GHC.Paths ( libdir )
import DynFlags

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
          insertTokens tokenKeys
          insertComments (concat $ Map.elems ghcComments)
          runWriterT $ flip runReaderT (initExportState ParsedStage modSum emptyStore) $ exportModule $ parsedSource p
          store <- execWriterT $ case renamedSource t of Just rs -> flip runReaderT (initExportState RenameStage modSum emptyStore) $ exportRnModule rs
          runWriterT $ flip runReaderT (initExportState TypedStage modSum store) $ exportTcModule $ typecheckedSource t
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