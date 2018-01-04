module Main where

import System.Environment
import Database.Selda
import Database.Selda.SQLite
import Language.Haskell.Heed.Refactor.RenameDefinition
import SrcLoc

main = do [span, newName] <- liftIO $ getArgs
          refactor span newName

tests = sequence_ $ [ putStrLn "=== 3:1-3:2 x"
                    , print =<< refactor "3:1-3:2" "x"
                    , putStrLn "=== 3:1-3:2 d"
                    , print =<< refactor "3:1-3:2" "d"
                    , putStrLn "=== 3:3-3:4 x"
                    , print =<< refactor "3:3-3:4" "x"
                    , putStrLn "=== 3:3-3:4 d"
                    , print =<< refactor "3:3-3:4" "d"
                    , putStrLn "=== 3:3-3:4 b"
                    , print =<< refactor "3:3-3:4" "b"
                    , putStrLn "=== 7:1-7:2 xx"
                    , print =<< refactor "7:1-7:2" "b"
                    , putStrLn "=== 10:1-10:2 xx"
                    , print =<< refactor "10:1-10:2" "b"
                    , putStrLn "=== 13:1-13:2 xx"
                    , print =<< refactor "13:1-13:2" "b"
                    ]

refactor :: String -> String -> IO (Either String [(RealSrcSpan, String)])
refactor span newName = withSQLite "haskell.db" $ renameDefinition span newName

