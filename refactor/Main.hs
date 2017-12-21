module Main where

import System.Environment
import Data.List.Split
import Database.HDBC
import Database.HDBC.Sqlite3

main :: IO ()
main = do [span, str] <- getArgs
          conn <- connectSqlite3 "..\\haskell.db"
          let (stRow, stCol, endRow, endCol) = readSpan span
          selectedNameRes 
            <- quickQuery' conn ("SELECT file, start_row, start_col, end_row, end_col, namespace, uniq"
                                    ++ " FROM name_infos"
                                    ++ " WHERE (start_row < ? OR (start_row = ? AND start_col <= ?))" 
                                    ++ " AND (end_row < ? OR (end_row = ? AND end_col >= ?))"
                                    ++ " LIMIT 1")
                                [toSql stRow, toSql stRow, toSql stCol, toSql endRow, toSql endRow, toSql endCol]
          let [file, stRow, stCol, endRow, endCol, namespace, uniq] = head selectedNameRes
          res <- quickQuery' conn ("SELECT start_row, start_col, end_row, end_col"
                                      ++ " FROM name_infos" 
                                      ++ " WHERE uniq = ?")
                                  [uniq]
          -- there is a clash if a renamed name cannot be distinguished from another one
          clash <- quickQuery' conn ("SELECT DISTINCT occ.start_row, occ.start_col" 
                                        ++ " FROM name_infos occ JOIN scopes sc JOIN scope_names snm"
                                                   -- the name is in the scope
                                        ++ " ON sc.scope_id = snm.scope_id"
                                                   -- occ is in the scope
                                        ++ " WHERE ((sc.start_row < occ.start_row OR (sc.start_row = occ.start_row AND sc.start_col <= occ.start_col))" 
                                               ++ " AND (sc.end_row < occ.end_row OR (sc.end_row = occ.end_row AND sc.end_col >= occ.end_col)))"
                                                                          -- the name found is the result of renaming, while the renamed is in scope
                                        ++ " AND occ.namespace = ? AND ((occ.name = ? AND snm.uniq = ?)" 
                                                   -- OR the name found will be renamed and there is a name in scope that is like the result of the renaming
                                        ++ " OR (occ.uniq = ? AND snm.name = ?));")
                               [ namespace, toSql str, uniq, uniq, toSql str]

          let rewrites = map (map fromSql) res
              conflicts = map (map fromSql) clash
          putStrLn $ show (rewrites :: [[Int]])
          putStrLn $ show (conflicts :: [[Int]])

readSpan :: String -> (String, String, String, String)
readSpan sp = case splitOneOf "-:" sp of [stRow, stCol, endRow, endCol] -> (stRow, stCol, endRow, endCol)
                                         [row, col] -> (row, col, row, col)
                                         _ -> error $ "Cannot read span: " ++ sp