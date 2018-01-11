{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Heed.Refactor.RenameDefinition where

import System.Environment
import Data.List.Split
import Database.Selda
import Database.Selda.SQLite

import Data.Text (pack, unpack, cons)
import SrcLoc
import Data.List
import Data.Char
import Data.Maybe
import FastString
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

import Core
import Language.Haskell.Heed.Schema

renameDefinition :: String -> String -> SeldaT IO (Either String [(RealSrcSpan, Either RealSrcSpan String)])
renameDefinition newName srcSpan = do
  let (stRow, stCol, endRow, endCol) = readSpan srcSpan
  selectedName <- query $ limit 0 1 $ do
    n <- select names
    node <- select nodes
    restrict $ node ! node_id .== n ! name_node
    restrict $ (int stRow .> node ! node_start_row
                  .|| int stRow .== node ! node_start_row .&& int stCol .>= node ! node_start_col)
    restrict $ (node ! node_end_row .> int endRow
                  .|| node ! node_end_row .== int endRow .&& node ! node_end_col .>= int endCol)
    return ( n ! name_uniq :*: n ! name_str :*: n ! name_namespace )

  case selectedName of
    [uniq :*: original :*: namespace] ->
      case newNameProblems (unpack original) newName (unpack namespace) of
        Nothing -> do
          associatedNames <- query $ do
            bnd <- select implicitBinds
            restrict $ (bnd ! imp_bind_lhs) .== text uniq
            return (bnd ! imp_bind_rhs)
          renameUnique newName original (uniq : associatedNames) namespace
        Just err -> return $ Left $ "The new name '" ++ newName ++ "' is not correct: " ++ err
    _ -> renameModule newName (stRow, stCol, endRow, endCol)

renameUnique :: String -> Text -> [Text] -> Text -> SeldaT IO (Either String [(RealSrcSpan, Either RealSrcSpan String)])
renameUnique newName original uniqs namespace = do
  renameRanges <- query $ do
    n <- select names
    nameNode <- select nodes
    unqualNode <- select nodes
    restrict $ nameNode ! node_id .== n ! name_node
                 .&& just (nameNode ! node_id) .== unqualNode ! node_parent
                 .&& unqualNode ! node_type .== int (typeId (undefined :: UnqualifiedName))
    restrict $ n ! name_uniq `isIn` map text uniqs
                 .&& n ! name_namespace .== text namespace
    return ( unqualNode ! node_file :*: unqualNode ! node_start_row :*: unqualNode ! node_start_col
               :*: unqualNode ! node_end_row :*: unqualNode ! node_end_col :*: n ! name_defining )

  align <- query $ distinct $ do
    let inRow node (fl :*: _ :*: _ :*: er :*: ec :*: _)
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

  let nameInScope node sc = do
        restrict $ (sc ! scope_start_row .< node ! node_start_row
                      .|| sc ! scope_start_row .== node ! node_start_row .&& sc ! scope_start_col .<= node ! node_start_col)
        restrict $ (node ! node_end_row .< sc ! scope_end_row
                      .|| node ! node_end_row .== sc ! scope_end_row .&& node ! node_end_col .<= sc ! scope_end_col)
  let isOuterScope sc sco  = do
        restrict $ sco ! scope_start_row .< sc ! scope_start_row
                     .|| sco ! scope_start_row .== sc ! scope_start_row .&& sco ! scope_start_col .<= sc ! scope_start_col
        restrict $ sc ! scope_end_row .< sco ! scope_end_row
                     .|| sc ! scope_end_row .== sco ! scope_end_row .&& sc ! scope_end_col .<= sco ! scope_end_col

  clashWithDefined <- query $ distinct $ do
    node <- select nodes
    occ <- select names
    sc <- select scopes
    sco <- select scopes
    snm <- select definitions
    snmo <- select definitions

    restrict $ node ! node_id .== occ ! name_node
    restrict $ just (sc ! scope_id) .== snm ! def_scope
    restrict $ just (sco ! scope_id) .== snmo ! def_scope

    nameInScope node sc
    nameInScope node sco
    isOuterScope sc sco

    -- the name found is the result of renaming, while the renamed is in scope  (and the name's definition is not closer to usage)
    restrict $ occ ! name_namespace .== text namespace
                 .&& occ ! name_str .== text (pack newName)
                 .&& snm ! def_uniq `isIn` map text uniqs
                 .&& snmo ! def_uniq .== occ ! name_uniq
    -- OR the name found will be renamed and there is a name in scope that is like the result of the renaming (and the renamed id's definition is not closer to usage)
                .|| occ ! name_uniq `isIn` map text uniqs
                     .&& not_ (occ ! name_defining)
                     .&& snm ! def_str .== text (pack newName)
                     .&& snmo ! def_uniq `isIn` map text uniqs
                     .&& snm ! def_namespace .== text namespace
    return ( node ! node_start_row :*: node ! node_start_col :*: snm ! def_uniq :*: snmo ! def_uniq :*: true )

  clashWithImported <- query $ distinct $ do
    node <- select nodes
    occ <- select names
    def <- select definitions
    sc <- select scopes
    mi <- select moduleImports

    hiding <- leftJoin (\(node :*: name) -> just node .== mi ! mi_node) $ do
      mih <- select moduleImportHiding
      node <- select nodes
      restrict $ mih ! mih_node .== node ! node_id
                  .&& mih ! mih_str .== text (pack newName)
      return (node ! node_id :*: mih ! mih_name)

    showing <- leftJoin (\(node :*: name) -> just node .== mi ! mi_node) $ do
      mis <- select moduleImportShowing
      node <- select nodes
      restrict $ mis ! mis_node .== node ! node_id
                  .&& mis ! mis_str .== text (pack newName)
      return (node ! node_id :*: mis ! mis_name)

    restrict $ node ! node_id .== occ ! name_node
                 .&& occ ! name_uniq `isIn` map text uniqs
                 .&& not_ (occ ! name_defining)
                 .&& not_ (mi ! mi_qualified) -- TODO: check for qualified conflicts
                 .&& isNull (first hiding)
                 .&& (not_ (mi ! mi_just_listed) .|| not_ (isNull (first showing)))
    nameInScope node sc
    restrict $ mi ! mi_scope_id .== sc ! scope_id .&& just (mi ! mi_module_id) .== def ! def_module
    restrict $ def ! def_str .== text (pack newName) .&& def ! def_namespace .== text namespace
    return ( node ! node_start_row :*: node ! node_start_col :*: occ ! name_uniq :*: def ! def_uniq :*: false )

  let clash = clashWithDefined ++ clashWithImported

  -- find the fields that could be merged with the name
  mergeableCandidate <- query $ do
    nCtor <- select ctorFields
    occCtor <- select ctorFields
    nTyp <- select typeCtors
    occTyp <- select typeCtors
    nType <- select types
    occType <- select types
    restrict $ nCtor ! cf_field `isIn` (map text uniqs)
                 .&& nCtor ! cf_constructor .== nTyp ! ct_ctor
                 .&& occCtor ! cf_constructor .== occTyp ! ct_ctor
                 .&& nTyp ! ct_type .== occTyp ! ct_type
                 .&& nTyp ! ct_ctor ./= occTyp ! ct_ctor
                 .&& occCtor ! cf_field .== occType ! type_name
                 .&& nCtor ! cf_field .== nType ! type_name
    return (occCtor ! cf_field :*: occType ! type_desc :*: nType ! type_desc )
  let mergeable = catMaybes $ map (\( name :*: t1 :*: t2 ) -> if read (unpack t1) `typeRepEq` read (unpack t2) then Just name else Nothing) mergeableCandidate
      isMergeable (r :*: c :*: n1 :*: n2 :*: _) = n1 `notElem` mergeable && n2 `notElem` mergeable
      isDefining ( _ :*: _ :*: _ :*: _ :*: _ :*: d ) = d
  case (or (map isDefining renameRanges) || unpack namespace == "type variable", filter isMergeable clash) of
    (False, _) -> return $ Left $ "Definition of name is not found."
    (True, []) -> do
      let nameLengthChange = length newName - length (unpack original)
          nameExtend = if nameLengthChange > 0 then nameLengthChange else 0
          nameShorten = if nameLengthChange < 0 then -nameLengthChange else 0
          nameRewriteChanges = map ((, Right newName) . resToSpan) renameRanges
          alignDistinct = nub $ sortOn second align
          realignChanges = concatMap (\(fl :*: sr :*: er) -> map ((, Right $ replicate nameExtend ' ') . updateEnd (updateCol (+ nameShorten)) . alignToSpan fl) [sr..er]) alignDistinct
      return $ Right (nameRewriteChanges ++ realignChanges)
    (True, clashes) -> return $ Left $ "Name clashes at: " ++ show clashes
  where
    resToSpan (file :*: sr :*: sc :*: er :*: ec :*: _)
      = mkRealSrcSpan (mkRealSrcLoc (mkFastString (unpack file)) sr sc) (mkRealSrcLoc (mkFastString (unpack file)) er ec)
    alignToSpan file sr = realSrcLocSpan (mkRealSrcLoc (mkFastString (unpack file)) sr 1)

renameModule :: String -> (Int, Int, Int, Int) -> SeldaT IO (Either String [(RealSrcSpan, Either RealSrcSpan String)])
renameModule newName (stRow, stCol, endRow, endCol) = do
  renameModuleName <- query $ do
    node <- select nodes
    attr <- select attributes
    restrict $ node ! node_type .== int (typeId (undefined :: ModuleName))
                 .&& attr ! container .== node ! node_id
                 .&& not_ (isNull (attr ! text_attribute))
    restrict $ (int stRow .> node ! node_start_row
                  .|| int stRow .== node ! node_start_row .&& int stCol .>= node ! node_start_col)
    restrict $ (node ! node_end_row .> int endRow
                  .|| node ! node_end_row .== int endRow .&& node ! node_end_col .>= int endCol)
    return $ attr ! text_attribute
  case renameModuleName of [ Just mn ] -> renameModuleStr mn newName
                           _           -> return $ Left $ "No name is selected."

renameModuleStr :: Text -> String -> SeldaT IO (Either String [(RealSrcSpan, Either RealSrcSpan String)])
renameModuleStr oldName newName = do
  rewritten <- query $ do
    node <- select nodes
    attr <- select attributes
    restrict $ node ! node_type `isIn` [ int (typeId (undefined :: ModuleName))
                                       , int (typeId (undefined :: Qualifiers)) ]
                 .&& attr ! container .== node ! node_id
                 .&& attr ! text_attribute .== just (text oldName)
    return ( node ! node_file :*: node ! node_start_row :*: node ! node_start_col
               :*: node ! node_end_row :*: node ! node_end_col )

  conflicts <- query $ do
    node <- select nodes
    attr <- select attributes
    restrict $ node ! node_type .== int (typeId (undefined :: ModuleName))
                 .&& attr ! container .== node ! node_id
                 .&& attr ! text_attribute .== just (text (pack newName))
    return ( node ! node_file :*: node ! node_start_row :*: node ! node_start_col
               :*: node ! node_end_row :*: node ! node_end_col )

  let rewriteChanges = map ((, Right newName) . resToSpan) rewritten
  case conflicts of [] -> return $ Right rewriteChanges
                    conflicts -> return $ Left $ "Conflicts found while renaming module: " ++ show conflicts
  where resToSpan (file :*: sr :*: sc :*: er :*: ec)
          = mkRealSrcSpan (mkRealSrcLoc (mkFastString (unpack file)) sr sc) (mkRealSrcLoc (mkFastString (unpack file)) er ec)

applyRewritings :: [(RealSrcSpan, Either RealSrcSpan String)] -> String -> String
applyRewritings [] s = s
applyRewritings rewrites@((aRange,_):_) s
  = applyRewritings' (map (\(sp, repl) -> (sp, either (extractSpan s) id repl)) $ sortOn fst rewrites)
                     (mkRealSrcLoc (srcSpanFile aRange) 1 1) s
  where extracted = catMaybes $ map (\case (_, Left rng) -> Just rng; _ -> Nothing) rewrites

extractSpan :: String -> RealSrcSpan -> String
extractSpan str sp = takeToPos start end "" . dropUntil fileStart start $ str
  where start = realSrcSpanStart sp
        end = realSrcSpanEnd sp
        fileStart = mkRealSrcLoc (srcSpanFile sp) 1 1

applyRewritings' :: [(RealSrcSpan, String)] -> RealSrcLoc -> String -> String
applyRewritings' ((replace, replacement):rest) loc s | realSrcSpanStart replace == loc
  = replacement ++ applyRewritings' rest (realSrcSpanEnd replace) (dropUntil loc (realSrcSpanEnd replace) s)
applyRewritings' replacements loc (c:rest) = c : applyRewritings' replacements (advanceSrcLoc loc c) rest
applyRewritings' _ _ [] = []

dropUntil loc end s | loc == end = s
dropUntil loc end (c:rest) = dropUntil (advanceSrcLoc loc c) end rest
dropUntil _ _ [] = []

takeToPos :: RealSrcLoc -> RealSrcLoc -> String -> String -> String
takeToPos loc end store s | loc == end = reverse store
takeToPos loc end store (c:rest) = takeToPos (advanceSrcLoc loc c) end (c:store) rest
takeToPos _ _ store [] = store

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

-- | Check if a given name is valid for a given kind of definition
newNameProblems :: String -> String -> String -> Maybe String
newNameProblems _ "" _ = Just "An empty name is not valid"
newNameProblems _ str _ | str `elem` reservedNames = Just $ "'" ++ str ++ "' is a reserved name"
  where -- TODO: names reserved by extensions
        reservedNames = [ "case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix"
                        , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"
                        , "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "[]"
                        ]
-- Operators that are data constructors (must start with ':')
newNameProblems orig newName "data constructor" | all isOperatorChar orig
  = case newName of (':' : nameRest) | all isOperatorChar nameRest -> Nothing
                    _ -> Just "The name must start with ':' and only contain operator characters."
-- Type families and synonyms that are operators (can start with ':')
newNameProblems orig newName "type constructor or class" | all isOperatorChar orig
  = if all isOperatorChar newName then Nothing
      else Just "The name must only contain operator characters."
-- Normal value operators (cannot start with ':')
newNameProblems orig newName "variable" | all isOperatorChar orig
  = case newName of (start : nameRest) | isOperatorChar start && start /= ':' && all isOperatorChar nameRest -> Nothing
                    _ -> Just "The name must start with ':' and only contain operator characters."
-- Data and type constructors (start with uppercase)
newNameProblems orig newName namespace | namespace `elem` ["data constructor", "type constructor or class"]
  = case newName of (start : nameRest) | isUpper start && isLetter start && all isIdChar nameRest -> Nothing
                    _ -> Just "The name must start with an uppercase letter, and only contain letters, digits, apostrhophe or underscore"
-- Variables and type variables (start with lowercase)
newNameProblems orig newName namespace | namespace `elem` ["variable", "type variable"]
  = case newName of (start : nameRest) | ((isLower start && isLetter start) || start == '\'' || start == '_') && all isIdChar nameRest -> Nothing
                    _ -> Just "The name of a value must start with lowercase, and only contain letters, digits, apostrhophe or underscore"
newNameProblems orig newName namespace = error $ "newNameProblems: " ++ orig ++ " " ++ newName ++ " " ++ namespace

isIdChar :: Char -> Bool
isIdChar c = isLetter c || isDigit c || c == '\'' || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = isPunctuation c || isSymbol c