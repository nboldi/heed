{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.Heed.Export.Utilities where

import GHC
import FastString
import Outputable (showSDocUnsafe, ppr)
import Name
import Id
import SrcLoc
import Outputable (Outputable(..))
import Bag

import Control.Exception
import Control.Monad.Reader
import Database.Selda
import Core
import Data.Data
import Data.Text (pack)

import Language.Haskell.Heed.Export.Schema (Schema(..))
import qualified Language.Haskell.Heed.Export.Schema as Schema

data ExportState = ExportState { parentData :: Maybe (RowID, Int)
                               , exportSyntax :: Bool
                               , compiledModule :: Module
                               , isDefining :: Bool
                               , scope :: RowID
                               }

initExportState :: Bool -> Module -> ExportState
initExportState exportSyntax mod = ExportState Nothing exportSyntax mod False undefined

type TrfType = ReaderT ExportState (SeldaT Ghc)

type Exporter n = n -> TrfType ()

class IsRdrName n where
  toRdrName :: n -> RdrName

class CompilationPhase n where
  getBindsAndSigs :: HsValBinds n -> ([LSig n], LHsBinds n)

instance CompilationPhase RdrName where
  getBindsAndSigs (ValBindsIn binds sigs) = (sigs, binds)
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsOut in parsed source"

instance CompilationPhase Name where
  getBindsAndSigs (ValBindsOut bindGroups sigs) = (sigs, unionManyBags (map snd bindGroups))
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsIn in renamed source"

instance CompilationPhase Id where
  getBindsAndSigs (ValBindsOut bindGroups _) = ([], unionManyBags (map snd bindGroups))
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsIn in renamed source"

class (DataId n, HasOccName n, HsHasName n, IsRdrName n, Outputable n, CompilationPhase n)
      => HsName n where
  exportName :: Exporter (Located n)
  exportOperator :: Exporter (Located n)
  exportRnName :: (forall x . HsName x => Exporter (Located x)) -> Located RdrName -> Exporter (Located (PostRn n n))
  exportNameOrRdrName :: (forall x . HsName x => Exporter (Located x)) -> Exporter (Located (NameOrRdrName n))
  exportFieldOccName :: Exporter (Located (FieldOcc n))

goInto :: Maybe RowID -> Int -> TrfType a -> TrfType a
goInto (Just id) ref = local (\s -> s { parentData = Just (id, ref) })
goInto Nothing _ = id

defining :: TrfType a -> TrfType a
defining = local $ \s -> s { isDefining = True }

writeInsert :: Schema s => s -> SrcSpan -> TrfType (Maybe RowID)
writeInsert ctor loc = do
  expSyntax <- asks exportSyntax
  if expSyntax then Just <$> writeInsert' ctor loc
               else return Nothing

writeInsert' :: Schema s => s -> SrcSpan -> TrfType RowID
writeInsert' ctor loc = do
  parentRef <- asks parentData
  let (file, start_row, start_col, end_row, end_col) = spanData loc
  lift $ insertWithPK nodes [ def :*: fmap fst parentRef :*: typeId ctor
                                :*: constrIndex (toConstr ctor) :*: pack file
                                :*: start_row :*: start_col :*: end_row :*: end_col
                                :*: fmap snd parentRef ]

writeIntAttribute :: Int -> TrfType ()
writeIntAttribute i = writeAttribute (Nothing :*: Just i :*: Nothing)

writeBoolAttribute :: Bool -> TrfType ()
writeBoolAttribute b = writeAttribute (Nothing :*: Just (if b then 1 else 0) :*: Nothing)

writeStringAttribute :: String -> TrfType ()
writeStringAttribute s = writeAttribute (Just (pack s) :*: Nothing :*: Nothing)

writeFractionalAttribute :: Double -> TrfType ()
writeFractionalAttribute f = writeAttribute (Nothing :*: Nothing :*: Just f)

writeAttribute :: ( Maybe Text :*: Maybe Int :*: Maybe Double ) -> TrfType ()
writeAttribute d = do
  pd <- asks parentData
  case pd of
    Just (parentId, parentRef) -> lift $ insert_ attributes [ parentId :*: parentRef :*: d ]
    Nothing -> return () -- not in export-syntax mode

lookupNameNode :: Text -> Int -> Int -> SeldaT Ghc [RowID]
lookupNameNode = prepared $ \file start_row start_col -> do
  n <- select nodes
  restrict $ file .== n ! node_file
              .&& start_row .== n ! node_start_row
              .&& start_col .== n ! node_start_col
              .&& (n ! node_type .== int (typeId (undefined :: Schema.Name))
                    .|| n ! node_type .== int (typeId (undefined :: Schema.Operator)))
  return (n ! node_id)

writeName :: SrcSpan -> Name -> TrfType ()
writeName sp name = do
  cm <- asks compiledModule
  sc <- asks scope
  defining <- asks isDefining
  let (file, start_row, start_col, _, _) = spanData sp
  [nodeId] <- lift $ lookupNameNode (pack file) start_row start_col

  let (d_file, d_start_row, d_start_col, d_end_row, d_end_col)
        = case nameSrcSpan name of RealSrcSpan rsp -> ( Just (unpackFS (srcSpanFile rsp)), Just (srcSpanStartLine rsp)
                                                      , Just (srcSpanStartCol rsp), Just (srcSpanEndLine rsp)
                                                      , Just (srcSpanEndCol rsp) )
                                   _               -> (Nothing, Nothing, Nothing, Nothing, Nothing)

      uniq = maybe (showSDocUnsafe (pprModule cm) ++ "." ++ nameStr ++ "?" ++ show (nameUnique name))
                   ((++ ("."++nameStr)) . showSDocUnsafe . pprModule)
                   (nameModule_maybe name)
      namespace = occNameSpace $ nameOccName name
      nameStr = occNameString $ nameOccName name
  lift $ insert_ names [ Just nodeId :*: sc :*: fmap pack d_file :*: d_start_row :*: d_start_col :*: d_end_row :*: d_end_col
                           :*: pack (showSDocUnsafe (pprNameSpace namespace)) :*: pack nameStr :*: pack uniq :*: defining ]

writeType :: SrcSpan -> Id -> TrfType ()
writeType sp id = do
  let (file, start_row, start_col, _, _) = spanData sp
      annot = showSDocUnsafe $ ppr $ idType id
  nodeIds <- lift $ lookupNameNode (pack file) start_row start_col
  case nodeIds of
    [nodeId] -> lift $ insert_ types [ nodeId :*: pack annot ]
    _ -> liftIO $ putStrLn $ "Could not insert type annotation '" ++ annot ++ "' on node at: " ++ show (file, start_row, start_col)


spanData :: SrcSpan -> (String, Int, Int, Int, Int)
spanData sp = case sp of RealSrcSpan rsp -> ( unpackFS (srcSpanFile rsp)
                                            , srcSpanStartLine rsp
                                            , srcSpanStartCol rsp
                                            , srcSpanEndLine rsp
                                            , srcSpanEndCol rsp )
                         _ -> ("", 0, 0, 0, 0)

combineLocated :: [Located a] -> SrcSpan
combineLocated = foldl combineSrcSpans noSrcSpan . map getLoc

combineSpans :: [SrcSpan] -> SrcSpan
combineSpans = foldl combineSrcSpans noSrcSpan

-------------------------------------------------------------------------

addToScope :: SrcSpan -> TrfType () -> TrfType ()
addToScope sp act = do
  expSyntax <- asks exportSyntax
  if expSyntax then act else doAddToScope sp act

doAddToScope :: SrcSpan -> TrfType () -> TrfType ()
doAddToScope sp act = do
  let (file, start_row, start_col, end_row, end_col) = spanData sp
  newScope <- lift $ insertWithPK scopes [ def :*: pack file :*: start_row :*: start_col :*: end_row :*: end_col ]
  local (\s -> s { scope = newScope}) act

scopedSequence :: (Exporter (Located e)) -> Exporter [Located e]
scopedSequence f (first:next:rest)
  = addToScope (combineLocated (next:rest)) (f first >> scopedSequence f (next:rest))
scopedSequence f elems = mapM_ f elems

--------------------------------------------------------------------------

export :: Schema c => c -> SrcSpan -> [(TrfType ())] -> TrfType ()
export ctor loc fields = do
  id <- writeInsert ctor loc
  mapM_ (\(i,val) -> goInto id i val) (zip [1..] fields)

exportError :: Data s => String -> s -> a
exportError exporter a = throw $ ExportException exporter (dataTypeOf a) (toConstr a)

data ExportException = ExportException String DataType Constr deriving Show

instance Exception ExportException where
  displayException (ExportException exporter typ ctor)
    = "Unexpected element while exporting: " ++ show ctor ++ " of type " ++ show typ ++ " with " ++ exporter

--------------------------------------------------------------------------

-- | Update column information in a source location
updateCol :: (Int -> Int) -> SrcLoc -> SrcLoc
updateCol _ loc@(UnhelpfulLoc _) = loc
updateCol f (RealSrcLoc loc) = mkSrcLoc (srcLocFile loc) (srcLocLine loc) (f $ srcLocCol loc)

-- | Update the start of the src span
updateStart :: (SrcLoc -> SrcLoc) -> SrcSpan -> SrcSpan
updateStart f sp = mkSrcSpan (f (srcSpanStart sp)) (srcSpanEnd sp)

-- | Update the end of the src span
updateEnd :: (SrcLoc -> SrcLoc) -> SrcSpan -> SrcSpan
updateEnd f sp = mkSrcSpan (srcSpanStart sp) (f (srcSpanEnd sp))

class HsHasName a where
  hsGetNames :: a -> [GHC.Name]

instance HsHasName RdrName where
  hsGetNames _ = []

instance HsHasName Name where
  hsGetNames n = [n]

instance HsHasName Id where
  hsGetNames n = [idName n]

instance HsHasName e => HsHasName [e] where
  hsGetNames es = concatMap hsGetNames es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames (L _ e) = hsGetNames e

instance HsHasName n => HsHasName (Pat n) where
  hsGetNames (VarPat id) = hsGetNames id

instance HsHasName n => HsHasName (HsBind n) where
  hsGetNames (FunBind {fun_id = lname}) = hsGetNames lname
