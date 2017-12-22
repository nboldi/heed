module Main where

import System.Environment
import Data.List.Split
import Database.Selda
import Database.Selda.SQLite

import Data.Text (pack)

import Core

main = do [span, newName] <- liftIO $ getArgs
          refactor span newName

tests = sequence_ $ [ putStrLn "=== 3:1-3:2 x"
                    , refactor "3:1-3:2" "x" 
                    , putStrLn "=== 3:1-3:2 d"
                    , refactor "3:1-3:2" "d" 
                    , putStrLn "=== 3:3-3:4 x"
                    , refactor "3:3-3:4" "x" 
                    , putStrLn "=== 3:3-3:4 d"
                    , refactor "3:3-3:4" "d"
                    , putStrLn "=== 3:3-3:4 b"
                    , refactor "3:3-3:4" "b" 
                    ]

refactor :: String -> String -> IO ()
refactor span newName = withSQLite "haskell.db" $ do
          let (stRow, stCol, endRow, endCol) = readSpan span
          
          selectedName <- query $ limit 0 1 $ do
            n <- select names
            node <- select nodes
            restrict $ just (node ! node_id) .== n ! name_node
            restrict $ (int stRow .< node ! node_start_row
                          .|| int stRow .== node ! node_start_row .&& int stCol .<= node ! node_start_col)
            restrict $ (node ! node_end_row .< int endRow 
                          .|| node ! node_end_row .== int endRow .&& node ! node_end_col .<= int endCol)
            return ( n ! name_uniq :*: n ! name_namespace )
          let uniq :*: namespace = head selectedName
          
          rewritten <- query $ do
            n <- select names
            node <- select nodes
            restrict $ just (node ! node_id) .== n ! name_node
            restrict $ n ! name_uniq .== text uniq
            return (node ! node_file :*: node ! node_start_row :*: node ! node_start_col :*: node ! node_end_row :*: node ! node_end_col )
          
          clash <- query $ distinct $ do
            node <- select nodes
            occ <- select names
            sc <- select scopes
            sco <- select scopes
            snm <- select names
            snmo <- select names
            restrict $ just (node ! node_id) .== occ ! name_node
            restrict $ snm ! name_defining .&& sc ! scope_id .== snm ! name_scope -- snm is defined in sc
            restrict $ snmo ! name_defining .&& sco ! scope_id .== snmo ! name_scope -- snmo is defined in sco
            let nameInScope occ sc = do
                  restrict $ (sc ! scope_start_row .< node ! node_start_row 
                                .|| sc ! scope_start_row .== node ! node_start_row .&& sc ! scope_start_col .<= node ! node_start_col)
                  restrict $ (node ! node_end_row .< sc ! scope_end_row
                                .|| node ! node_end_row .== sc ! scope_end_row .&& node ! node_end_col .<= sc ! scope_end_col)
                isOuterScope sc sco  = do
                  restrict $ sco ! scope_start_row .< sc ! scope_start_row 
                               .|| sco ! scope_start_row .== sc ! scope_start_row .&& sco ! scope_start_col .<= sc ! scope_start_col
                  restrict $ sc ! scope_end_row .< sco ! scope_end_row
                               .|| sc ! scope_end_row .== sco ! scope_end_row .&& sc ! scope_end_col .<= sco ! scope_end_col
            nameInScope occ sc
            nameInScope occ sco
            isOuterScope sc sco

            -- the name found is the result of renaming, while the renamed is in scope  (and the name's definition is not closer to usage)
            restrict $ occ ! name_namespace .== text namespace .&& occ ! name_str .== text (pack newName) .&& snm ! name_uniq .== text uniq 
                         .&& snmo ! name_uniq .== occ ! name_uniq
            -- OR the name found will be renamed and there is a name in scope that is like the result of the renaming (and the renamed id's definition is not closer to usage)
                        .|| occ ! name_uniq .== text uniq .&& snm ! name_str .== text (pack newName)
                             .&& snmo ! name_uniq .== text uniq
            return ( node ! node_start_row :*: node ! node_start_col :*: snm ! name_str :*: snmo ! name_str )
                          
              -- conflicts = map (map fromSql) clash
          liftIO $ putStrLn $ show uniq
          liftIO $ putStrLn $ show selectedName
          liftIO $ putStrLn $ show rewritten
          liftIO $ putStrLn $ show clash

readSpan :: String -> (Int, Int, Int, Int)
readSpan sp = case splitOneOf "-:" sp of [stRow, stCol, endRow, endCol] -> (read stRow, read stCol, read endRow, read endCol)
                                         [row, col] -> (read row, read col, read row, read col)
                                         _ -> error $ "Cannot read span: " ++ sp