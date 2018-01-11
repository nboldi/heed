module Main where

import System.Environment
import Database.Selda
import Database.Selda.SQLite
import Language.Haskell.Heed.Refactor.RenameDefinition
import Language.Haskell.Heed.Refactor.OrganizeImports
import SrcLoc

main = do args <- liftIO $ getArgs
          res <- withSQLite "haskell.db"
            $ case args of ["OrganizeImports"] -> organizeImports
                           ["Rename", span, newName] -> renameDefinition span newName
          print res

