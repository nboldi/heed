{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, TypeSynonymInstances, FlexibleInstances, BangPatterns, OverloadedStrings #-}
module Main where

import Control.Monad.Reader
import Database.Selda
import Database.Selda.SQLite
import Data.Maybe
import qualified Data.Map as Map
import Data.Text (pack)
import System.Environment

import GHC
import GHC.Paths ( libdir )
import DynFlags

import Core
import Language.Haskell.Heed.Export.Modules
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Lexical

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
          flip runReaderT es $ exportModule $ parsedSource p
          case renamedSource t of Just rs -> flip runReaderT (es {exportSyntax = False}) $ exportRnModule rs

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


