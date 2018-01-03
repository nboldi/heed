module Main where

import System.Environment
import Language.Haskell.Heed.Export.Export

main :: IO ()
main = do args <- getArgs
          case args of "no-export":root:mn:_ -> exportSrcFile root mn False
                       "no-export":mn:_      -> exportSrcFile "." mn False
                       root:mn:_             -> exportSrcFile root mn True
                       mn:_                  -> exportSrcFile "." mn True
                       _ -> error "export needs at least one argument, the module name"



