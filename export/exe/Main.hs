module Main where

import System.Environment
import Language.Haskell.Heed.Export.Export

main :: IO ()
main = do args <- getArgs
          case args of "no-export":db:root:mn:_ -> exportSrcFile db root mn False
                       "no-export":db:mn:_      -> exportSrcFile db "." mn False
                       db:root:mn:_             -> exportSrcFile db root mn True
                       db:mn:_                  -> exportSrcFile db "." mn True
                       _ -> error "export arguments: 'no-export'?, db file, root?, module name"



