module Main where

import GHC.Paths
import System.Process
import System.Environment
import System.Exit
import Data.List

main = do
  args <- getArgs
  case partition isDisabledArg args of
    (filtered, passed) -> do
      let ghcExe = maybe ghc (drop (length ugOpt)) (find (ugOpt `isPrefixOf`) filtered)
      callProcess ghcExe (passed ++ [ "--frontend", "Language.Haskell.Heed.Export.Plugin" ])
  where isDisabledArg e = e `elem` ["--make", "--interactive", "--print-libdir"]
                            || ugOpt `isPrefixOf` e
        ugOpt = "--use-ghc="