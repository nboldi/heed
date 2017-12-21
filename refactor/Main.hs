module Main where

import System.Environment
import Data.List.Split
import Database.Selda
import Database.Selda.SQLite

import Data.Text (pack)

import Core

main = do [span, newName] <- liftIO $ getArgs
          refactor span newName

refactor :: String -> String -> IO ()
refactor span newName = withSQLite "haskell.db" $ do
          let (stRow, stCol, endRow, endCol) = readSpan span
          
          selectedName <- query $ limit 0 1 $ do
            n <- select names
            restrict $ (int stRow .< n ! name_start_row
                          .|| int stRow .== n ! name_start_row .&& int stCol .<= n ! name_start_col)
            restrict $ (n ! name_end_row .< int endRow 
                          .|| n ! name_end_row .== int endRow .&& n ! name_end_col .<= int endCol)
            return ( n ! name_uniq :*: n ! name_namespace )
          let uniq :*: namespace = head selectedName
          
          rewritten <- query $ do
            n <- select names
            restrict $ n ! name_uniq .== text uniq
            return (n ! name_file :*: n ! name_start_row :*: n ! name_start_col :*: n ! name_end_row :*: n ! name_end_col )
          
          clash <- query $ distinct $ do
            occ <- select names
            sc <- select scopes
            sco <- select scopes
            snm <- select scopeNames
            snmo <- select scopeNames
            restrict $ sc ! scope_id .== snm ! scope_name_id -- snm is visible in sc
            restrict $ sco ! scope_id .== snmo ! scope_name_id -- snmo is visible in sco
            let nameInScope occ sc = do
                  restrict $ (sc ! scope_start_row .< occ ! name_start_row 
                                .|| sc ! scope_start_row .== occ ! name_start_row .&& sc ! scope_start_col .<= occ ! name_start_col)
                  restrict $ (occ ! name_end_row .< sc ! scope_end_row
                                .|| occ ! name_end_row .== sc ! scope_end_row .&& occ ! name_end_col .<= sc ! scope_end_col)
                isOuterScope sc sco  = do
                  restrict $ sco ! scope_start_row .< sc ! scope_start_row 
                               .|| sco ! scope_start_row .== sc ! scope_start_row .&& sco ! scope_start_col .<= sc ! scope_start_col
                  restrict $ sc ! scope_end_row .< sco ! scope_end_row
                               .|| sc ! scope_end_row .== sco ! scope_end_row .&& sc ! scope_end_col .<= sco ! scope_end_col
            nameInScope occ sc
            nameInScope occ sco
            isOuterScope sc sco

            -- the name found is the result of renaming, while the renamed is in scope  (and the name's definition is not closer to usage)
            restrict $ occ ! name_namespace .== text namespace .&& occ ! name_str .== text (pack newName) .&& snm ! scope_uniq .== text uniq 
                         .&& snmo ! scope_uniq .== occ ! name_uniq
            -- OR the name found will be renamed and there is a name in scope that is like the result of the renaming (and the renamed id's definition is not closer to usage)
                        .|| occ ! name_uniq .== text uniq .&& snm ! scope_name .== text (pack newName)
                             .&& snmo ! scope_uniq .== text uniq
            return ( occ ! name_start_row :*: occ ! name_start_col )
                          
              -- conflicts = map (map fromSql) clash
          liftIO $ putStrLn $ show uniq
          liftIO $ putStrLn $ show rewritten
          liftIO $ putStrLn $ show clash

readSpan :: String -> (Int, Int, Int, Int)
readSpan sp = case splitOneOf "-:" sp of [stRow, stCol, endRow, endCol] -> (read stRow, read stCol, read endRow, read endCol)
                                         [row, col] -> (read row, read col, read row, read col)
                                         _ -> error $ "Cannot read span: " ++ sp