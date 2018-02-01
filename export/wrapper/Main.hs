module Main where

import GHC.Paths
import System.Process
import System.Environment
import System.Exit
import Data.List
import Data.Maybe


main = do
  args <- getArgs
  -- let script = find ("-ghci-script" `isInfixOf`) args
  -- case script of Just s -> readFile (drop (length "-ghci-script=") s) >>= putStrLn
  --                Nothing -> putStrLn "No ghci-script"

  let frontendPos = findIndex ("-ffrontend-opt" `isInfixOf`) args
      frontendOpt = frontendPos >>= \p -> listToMaybe (drop (p + 1) args)
      args' = case frontendPos of Just p -> take p args ++ drop (p + 2) args
                                  Nothing -> args
      ghcExe = maybe ghc (drop (length ugOpt)) (find (ugOpt `isPrefixOf`) args)
  case (frontendOpt, partition isDisabledArg args') of
    (Just db, (filtered, passed)) -> do
      callProcess ghcExe (passed ++ [ "--frontend", "Language.Haskell.Heed.Export.Plugin"
                                    , "-plugin-package", "heed-export-0.1.0.0" ]
                                 ++ ["-ffrontend-opt", intercalate " " (db : filtered)]
                                 ++ [ a | a <- ["-user-package-db"], isInteractive filtered ])
    _ -> callProcess ghcExe args
  where isDisabledArg e = e `elem` ["--make", "--interactive", "--print-libdir"]
                            || ugOpt `isPrefixOf` e
        ugOpt = "--use-ghc="
        isInteractive args = "--interactive" `elem` args