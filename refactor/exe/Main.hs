module Main where

import System.Environment
import Database.Selda
import Database.Selda.SQLite
import Language.Haskell.Heed.Refactor.RenameDefinition
import Language.Haskell.Heed.Refactor.OrganizeImports
import SrcLoc

main = do args <- liftIO $ getArgs
          res <- case args of
            [dbFile, "OrganizeImports"] -> withSQLite dbFile $ organizeImports
            [dbFile, "Rename", file, span, newName] -> withSQLite dbFile $ renameDefinition file span newName
            _ -> error $ "Arguments not valid: " ++ show args
          print res

