module Main where

import System.Environment
import Language.Haskell.Heed.Export.Export

main :: IO ()
main = do args <- getArgs
          case args of "no-export":mn:_ -> exportSrcFile "." mn False
                       mn:_             -> exportSrcFile "." mn True
                       _ -> error "export needs at least one argument, the module name"



