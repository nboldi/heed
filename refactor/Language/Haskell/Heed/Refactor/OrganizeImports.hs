{-# LANGUAGE TypeOperators #-}
module Language.Haskell.Heed.Refactor.OrganizeImports where

import Database.Selda
import Data.Text (pack, unpack)
import Data.Maybe
import Data.List as List
import Data.Function (on)
import Core
import SrcLoc
import FastString
import Language.Haskell.Heed.Schema

organizeImports :: FilePath -> SeldaT IO (Either String [(RealSrcSpan, Either RealSrcSpan String)])
organizeImports fileName = do
  imports <- query $ do
    impDecl <- select nodes
    modName <- select nodes
    attr <- select attributes
    restrict $ modName ! node_parent .== just (impDecl ! node_id)
                .&& modName ! parent_handle .== just (int 5)
    restrict $ impDecl ! node_file `like` text (pack ("%" ++ fileName))
               .&& impDecl ! node_type .== int (typeId (undefined :: ImportDecl))
               .&& modName ! node_type .== int (typeId (undefined :: ModuleName))
               .&& attr ! container .== modName ! node_id
               .&& not_ (isNull (attr ! text_attribute))
    return ( impDecl ! node_file :*: impDecl ! node_start_row :*: impDecl ! node_start_col
               :*: impDecl ! node_end_row :*: impDecl ! node_end_col :*: attr ! text_attribute :*: impDecl ! node_id )
    -- TODO: extend to accomodate comments

  let groupedImports = groupByTrans (\i1 i2 -> fourth i1 + 1 == second i2) $ sortOn second imports

  potentialImports <- query $ distinct $ do
    node <- select nodes
    mi <- select moduleImports
    def <- select definitions
    usedNames <- leftJoin (\u -> u .== def ! def_uniq) $ do
      name <- select names
      restrict $ not_ (name ! name_defining)
      return ( name ! name_uniq )
    restrict $ not_ (isNull usedNames)
    restrict $ just (mi ! mi_module_id) .== def ! def_module
                 .&& just (node ! node_id) .== mi ! mi_node
    return ( def ! def_uniq :*: node ! node_id )

  liftIO $ print $ imports
  liftIO $ print $ groupedImports
  liftIO $ print $ concatMap sortNodes groupedImports
  liftIO $ print $ potentialImports
  return $ Right []

sortNodes :: [Text :*: Int :*: Int :*: Int :*: Int :*: Maybe Text :*: RowID] -> [(RealSrcSpan, Either RealSrcSpan String)]
sortNodes nodes = map (\(old,new) -> (pos (nodes !! old), Left (pos (nodes !! new))) ) permutation
  where permutation = zip [0..] $ map snd $ sortBy (compare `on` fst) $ zip texts [0..]
        texts = map (fromMaybe (error "sortNodes: Nothing received") . sixth) nodes
        pos (fl :*: sr :*: sc :*: er :*: ec :*: _)
         = mkRealSrcSpan (mkRealSrcLoc (mkFastString $ unpack fl) sr sc)
                         (mkRealSrcLoc (mkFastString $ unpack fl) er ec)

groupByTrans :: (a -> a -> Bool) -> [a] -> [[a]]
groupByTrans _ [] = []
groupByTrans pred (x:xs) = case doGroup [x] xs of (gr,rest) -> gr : groupByTrans pred rest
  where doGroup (last:coll) (next:rest) | pred last next = doGroup (next:last:coll) rest
        doGroup prev toGo                                = (reverse prev, toGo)



