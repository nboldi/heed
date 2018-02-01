module Main where

import GHC.Paths
import System.Process
import System.Environment
import System.Exit
import Data.List
import Data.Maybe


main = do
  args <- getArgs
  let script = find ("-ghci-script" `isInfixOf`) args
  ghciScript <- case script of Just s -> readFile (drop (length "-ghci-script=") s)
                               Nothing -> return ""
  let scriptLines = lines ghciScript
      addLine = find (":add " `isInfixOf`) scriptLines
      modules = words $ maybe "" (drop (length ":add ")) addLine

  let ghcExe = maybe ghc (drop (length ugOpt)) (find (ugOpt `isPrefixOf`) args)
  case partition isDisabledArg args of
    (filtered, passed) -> do
      callProcess ghcExe (passed ++ modules
                                 ++ [ "--frontend", "Language.Haskell.Heed.Export.Plugin"
                                    , "-plugin-package", "heed-export" ] )
  where isDisabledArg e = e `elem` ["--make", "--interactive", "--print-libdir"]
                            || ugOpt `isPrefixOf` e
        ugOpt = "--use-ghc="
        isInteractive args = "--interactive" `elem` args