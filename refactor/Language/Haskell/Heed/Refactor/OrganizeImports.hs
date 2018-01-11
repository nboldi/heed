module Language.Haskell.Heed.Refactor.OrganizeImports where

import Database.Selda
import Core
import SrcLoc
import Language.Haskell.Heed.Schema

organizeImports :: SeldaT IO (Either String [(RealSrcSpan, Either RealSrcSpan String)])
organizeImports = do
  imports <- query $ do
    node <- select nodes
    restrict $ node ! node_type .== int (typeId (undefined :: ImportDecl))
    return ( node ! node_start_row :*: node ! node_start_col :*: node ! node_end_row :*: node ! node_end_col )
  liftIO $ putStrLn $ show imports
  usedNames <- query $ do
    name <- select names
    restrict $ not_ (name ! name_defining)
    return ( name ! name_uniq )
  liftIO $ putStrLn $ show usedNames

  return $ Right []
