module Main where

import System.Environment

import Language.Haskell.Heed.Export.Export

main = do args <- getArgs
          case args of
            [db, root, mod]                -> exportSrcFile db root mod True
            [db, root, mod, "--no-export"] -> exportSrcFile db root mod False
            _                              -> error $ "Wrong arguments: " ++ show args