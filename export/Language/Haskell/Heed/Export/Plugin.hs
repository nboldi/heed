module Language.Haskell.Heed.Export.Plugin where

import BasicTypes
import GHC
import GhcMake
import GhcMonad
import HscMain
import DynFlags
import Plugins
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.IORef
import Control.Monad.IO.Class
import Outputable
import System.Exit
import Data.List
import DriverPhases
import Language.Haskell.Heed.Export.Export

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin { frontend = pluginAction }

pluginAction :: [CommandLineOption] -> [(String, Maybe Phase)] -> Ghc ()
pluginAction pluginArgs args = do
  case pluginArgs of
    [dbPath] -> do
      dflags0 <- getDynFlags
      -- NB: frontend plugin defaults to OneShot
      _ <- GHC.setSessionDynFlags dflags0 { ghcMode = CompManager }
      dflags <- getDynFlags
      let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget args
      targets <- mapM (uncurry GHC.guessTarget) hs_srcs
      GHC.setTargets targets
      mod_graph <- depanal [] False
      ok_flag <- load' LoadAllTargets (Just (exportMessager dbPath)) mod_graph
      when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    _ -> liftIO $ do
      putStrLn $ "The frontend plugin Language.Haskell.Heed.Export.Plugin needs 1 argument, the filepath of "
                    ++ " the database file to use. Please use -ffrontend-opt to provide this parameter."
      exitWith (ExitFailure 1)

exportMessager :: FilePath -> Messager
exportMessager db env i r ms = do
  putStrLn $ showSDocUnsafe $ ppr (ms_mod ms)
  print (isJust $ ms_parsed_mod ms)
  sess <- Session <$> newIORef env
  flip reflectGhc sess (exportModSummary db ms)
  batchMsg env i r ms
