module Language.Haskell.Heed.Export.Plugin where

import BasicTypes
import GHC
import GhcMake
import GhcMonad
import HscMain
import HscTypes
import TcRnTypes
import DynFlags
import Plugins
import Hooks
import Control.Monad
import Control.Concurrent.MVar
import Control.Applicative
import Data.Maybe
import Data.IORef
import Control.Monad.IO.Class
import Outputable
import ErrUtils
import System.Exit
import Data.List
import DriverPhases
import Language.Haskell.Heed.Export.Export

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin { frontend = pluginAction }

pluginAction :: [CommandLineOption] -> [(String, Maybe Phase)] -> Ghc ()
pluginAction pluginArgs args = do
  -- NB: frontend plugin defaults to OneShot
  dflags <- getDynFlags
  mv <- liftIO (newMVar [])
  let dflags' = (\df -> df { hooks = (hooks df) { hscFrontendHook = Just $ \ms -> myHscFrontend mv ms } })
                 . (\df -> df { ghcMode = CompManager })
                 $ dflags
  _ <- GHC.setSessionDynFlags dflags'
  let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget args
  targets <- mapM (uncurry GHC.guessTarget) hs_srcs
  GHC.setTargets targets
  ok_flag <- load LoadAllTargets
  mods <- liftIO $ takeMVar mv
  mapM_ exportModStuff mods
  when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
  exportGlobalStuff

myHscFrontend :: MVar [ModRecord] -> ModSummary -> Hsc FrontendResult
myHscFrontend mv mod_summary = do
  p <- hscParse' mod_summary
  env <- getHscEnv
  (gbl, _) <- liftIO $ hscTypecheckRename env mod_summary p
  liftIO $ modifyMVar_ mv ( return . ((mod_summary, p, gbl):) )
  return $ FrontendTypecheck gbl

