{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
module Language.Haskell.Heed.Refactor.RenameDefinition where

import System.Environment
import Data.List.Split
import Database.Selda
import Database.Selda.SQLite

import Data.Text (pack, unpack, cons)
import SrcLoc
import Data.List
import FastString
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

import Core
import Language.Haskell.Heed.Schema

renameDefinition :: String -> String -> SeldaT IO (Either String [(RealSrcSpan, String)])
renameDefinition newName srcSpan = do
  let (stRow, stCol, endRow, endCol) = readSpan srcSpan
  selectedName <- query $ limit 0 1 $ do
    n <- select names
    node <- select nodes
    restrict $ just (node ! node_id) .== n ! name_node
    restrict $ (int stRow .> node ! node_start_row
                  .|| int stRow .== node ! node_start_row .&& int stCol .>= node ! node_start_col)
    restrict $ (node ! node_end_row .> int endRow
                  .|| node ! node_end_row .== int endRow .&& node ! node_end_col .>= int endCol)
    return ( n ! name_uniq :*: n ! name_str :*: n ! name_namespace )

  case selectedName of
    [uniq :*: original :*: namespace] -> do
      associatedNames <- query $ do
        bnd <- select implicitBinds
        restrict $ (bnd ! imp_bind_lhs) .== text uniq
        return (bnd ! imp_bind_rhs)
      -- liftIO $ putStrLn $ "uniq: " ++ show uniq
      -- liftIO $ putStrLn $ "associatedNames: " ++ show associatedNames
      renameUnique newName original (uniq : associatedNames) namespace
    _ -> return $ Left $ "No name found at selection: " ++ show srcSpan

renameUnique :: String -> Text -> [Text] -> Text -> SeldaT IO (Either String [(RealSrcSpan, String)])
renameUnique newName original uniqs namespace = do
  renameRanges <- query $ do
    n <- select names
    nameNode <- select nodes
    unqualNode <- select nodes
    restrict $ just (nameNode ! node_id) .== n ! name_node
                 .&& just (nameNode ! node_id) .== unqualNode ! node_parent
                 .&& unqualNode ! node_type .== int (typeId (undefined :: UnqualifiedName))
    restrict $ n ! name_uniq `isIn` map text uniqs
                 .&& n ! name_namespace .== text namespace
    return ( unqualNode ! node_file :*: unqualNode ! node_start_row :*: unqualNode ! node_start_col
               :*: unqualNode ! node_end_row :*: unqualNode ! node_end_col )
  -- liftIO $ putStrLn $ "renameRanges: " ++ show renameRanges

  align <- query $ distinct $ do
    let inRow node (fl :*: _ :*: _ :*: er :*: ec)
          = node ! node_file .== text fl .&& node ! node_start_row .== int er .&& node ! node_start_col .> int ec
              .&& node ! node_end_row .> node ! node_start_row
    node <- select nodes
    restrict $ foldl (.||) false (map (inRow node) renameRanges)

    let nodeIsInSubseqRow n = n ! node_file .== node ! node_file
                                .&& n ! node_start_row .> node ! node_start_row
                                .&& n ! node_end_row .<= node ! node_end_row
                                .&& n ! node_start_col .<= node ! node_start_col
        commentIsInSubseqRow n = n ! comment_file .== node ! node_file
                                   .&& n ! comment_start_row .> node ! node_start_row
                                   .&& n ! comment_end_row .<= node ! node_end_row
                                   .&& n ! comment_start_col .<= node ! node_start_col
        tokenIsInSubseqRow n = n ! token_file .== node ! node_file
                                 .&& n ! token_start_row .> node ! node_start_row
                                 .&& n ! token_end_row .<= node ! node_end_row
                                 .&& n ! token_start_col .<= node ! node_start_col
    nodesBeforeCol <- leftJoin nodeIsInSubseqRow (select nodes)
    commentBeforeCol <- leftJoin commentIsInSubseqRow (select comments)
    tokenBeforeCol <- leftJoin tokenIsInSubseqRow (select tokens)

    restrict $ isNull (first nodesBeforeCol) .&& isNull (first commentBeforeCol) .&& isNull (first tokenBeforeCol)
    return (node ! node_file :*: (node ! node_start_row) + 1 :*: node ! node_end_row )

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
    restrict $ occ ! name_namespace .== text namespace .&& occ ! name_str .== text (pack newName) .&& snm ! name_uniq `isIn` map text uniqs
                 .&& snmo ! name_uniq .== occ ! name_uniq
    -- OR the name found will be renamed and there is a name in scope that is like the result of the renaming (and the renamed id's definition is not closer to usage)
                .|| occ ! name_uniq `isIn` map text uniqs .&& snm ! name_str .== text (pack newName)
                     .&& snmo ! name_uniq `isIn` map text uniqs .&& snm ! name_namespace .== text namespace
    return ( node ! node_start_row :*: node ! node_start_col :*: snm ! name_str :*: snmo ! name_str )

  case clash of
    [] -> do
      let nameLengthChange = length newName - length (unpack original)
          nameExtend = if nameLengthChange > 0 then nameLengthChange else 0
          nameShorten = if nameLengthChange < 0 then -nameLengthChange else 0
          nameRewriteChanges = map ((,newName) . resToSpan) renameRanges
          alignDistinct = nub $ sortOn second align
          realignChanges = concatMap (\(fl :*: sr :*: er) -> map ((, replicate nameExtend ' ') . updateEnd (updateCol (+ nameShorten)) . alignToSpan fl) [sr..er]) alignDistinct
      return $ Right (nameRewriteChanges ++ realignChanges)
    clashes -> return $ Left $ "Name clashes at: " ++ show clashes
  where
    resToSpan (file :*: sr :*: sc :*: er :*: ec)
      = mkRealSrcSpan (mkRealSrcLoc (mkFastString (unpack file)) sr sc) (mkRealSrcLoc (mkFastString (unpack file)) er ec)
    alignToSpan file sr = realSrcLocSpan (mkRealSrcLoc (mkFastString (unpack file)) sr 1)

applyRewritings :: [(RealSrcSpan, String)] -> String -> String
applyRewritings [] = id
applyRewritings rewrites@((aRange,_):_) = applyRewritings' (sortOn fst rewrites) (mkRealSrcLoc (srcSpanFile aRange) 1 1)

applyRewritings' :: [(RealSrcSpan, String)] -> RealSrcLoc -> String -> String
applyRewritings' ((replace, replacement):rest) loc s | realSrcSpanStart replace == loc
  = replacement ++ applyRewritings' rest (realSrcSpanEnd replace) (dropUntil loc (realSrcSpanEnd replace) s)
  where dropUntil loc end s | loc == end = s
        dropUntil loc end (c:rest) = dropUntil (advanceSrcLoc loc c) end rest
        dropUntil _ _ [] = []
applyRewritings' replacements loc (c:rest) = c : applyRewritings' replacements (advanceSrcLoc loc c) rest
applyRewritings' _ _ [] = []

readSpan :: String -> (Int, Int, Int, Int)
readSpan sp = case splitOneOf "-:" sp of [stRow, stCol, endRow, endCol] -> (read stRow, read stCol, read endRow, read endCol)
                                         [row, col] -> (read row, read col, read row, read col)
                                         _ -> error $ "Cannot read span: " ++ sp

-- | Update column information in a source location
updateCol :: (Int -> Int) -> RealSrcLoc -> RealSrcLoc
updateCol f loc = mkRealSrcLoc (srcLocFile loc) (srcLocLine loc) (f $ srcLocCol loc)

-- | Update the start of the src span
updateStart :: (RealSrcLoc -> RealSrcLoc) -> RealSrcSpan -> RealSrcSpan
updateStart f sp = mkRealSrcSpan (f (realSrcSpanStart sp)) (realSrcSpanEnd sp)

-- | Update the end of the src span
updateEnd :: (RealSrcLoc -> RealSrcLoc) -> RealSrcSpan -> RealSrcSpan
updateEnd f sp = mkRealSrcSpan (realSrcSpanStart sp) (f (realSrcSpanEnd sp))