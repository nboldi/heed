{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.Heed.Export.Utilities where

import GHC
import FastString
import Outputable (showSDocUnsafe, ppr)
import Name
import Id
import SrcLoc
import Outputable (Outputable(..), OutputableBndr)
import Bag
import HscTypes

import System.Directory
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Writer
import Database.Selda
import Core
import Data.Data
import Data.Text (pack)

import Language.Haskell.Heed.Schema (Schema(..))
import qualified Language.Haskell.Heed.Schema as Schema

data ExportState = ExportState { parentData :: Maybe (RowID, Int)
                               , exportStage :: ExportStage
                               , compiledModule :: ModSummary
                               , isDefining :: Bool
                               , scope :: Maybe RowID
                               , ambiguousNames :: [(SrcSpan, RowID)]
                               }

data ExportStore = ExportStore [(SrcSpan, RowID)]

emptyStore :: ExportStore
emptyStore = ExportStore []

instance Monoid ExportStore where
  mempty = ExportStore []
  mappend (ExportStore an1) (ExportStore an2) = ExportStore (an1 ++ an2)

data ExportStage = ParsedStage | RenameStage | TypedStage deriving (Eq, Show)

liftSelda :: SeldaT Ghc a -> TrfType a
liftSelda = lift . lift

initExportState :: ExportStage -> ModSummary -> ExportStore -> ExportState
initExportState stage mod (ExportStore amb) = ExportState Nothing stage mod False Nothing amb

type TrfType = ReaderT ExportState (WriterT ExportStore (SeldaT Ghc))

type Exporter n = n -> TrfType ()

class IsRdrName n where
  toRdrName :: n -> RdrName

class (OutputableBndr n, OutputableBndr (NameOrRdrName n), HsHasName (FieldOcc n)) => CompilationPhase n where
  getBindsAndSigs :: HsValBinds n -> ([LSig n], LHsBinds n)

instance CompilationPhase RdrName where
  getBindsAndSigs (ValBindsIn binds sigs) = (sigs, binds)
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsOut in parsed source"

instance CompilationPhase GHC.Name where
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
  exportAmbiguous :: (forall x . HsName x => Exporter (Located x)) -> Exporter (Located (AmbiguousFieldOcc n))

goInto :: Maybe RowID -> Int -> TrfType a -> TrfType a
goInto (Just id) ref = local (\s -> s { parentData = Just (id, ref) })
goInto Nothing _ = id

defining :: TrfType a -> TrfType a
defining = local $ \s -> s { isDefining = True }

writeInsert :: Schema s => s -> SrcSpan -> TrfType (Maybe RowID)
writeInsert ctor loc = do
  stage <- asks exportStage
  if stage == ParsedStage then Just <$> writeInsert' ctor loc
                          else return Nothing

writeInsert' :: Schema s => s -> SrcSpan -> TrfType RowID
writeInsert' ctor loc = do
  parentRef <- asks parentData
  let (file, start_row, start_col, end_row, end_col) = spanData loc
  liftSelda $ insertWithPK nodes [ def :*: fmap fst parentRef :*: typeId ctor
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
    Just (parentId, parentRef) -> liftSelda $ insert_ attributes [ parentId :*: parentRef :*: d ]
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

writeModule :: TrfType ()
writeModule = do
  ms <- asks compiledModule
  let mod = ms_mod ms
  sourcePath <- liftIO $ makeAbsolute (msHsFilePath ms)
  liftSelda $ insert_ modules [ def :*: pack (moduleNameString $ moduleName mod)
                                    :*: pack (show $ moduleUnitId mod)
                                    :*: pack sourcePath ]

writeName :: SrcSpan -> GHC.Name -> TrfType ()
writeName sp name = do
  sc <- asks scope
  case sc of
    Just scope -> doWriteName sp name scope
    Nothing -> return ()

doWriteName :: SrcSpan -> GHC.Name -> RowID -> TrfType ()
doWriteName sp name scope = do
  cm <- asks (ms_mod . compiledModule)
  defining <- asks isDefining
  let (file, start_row, start_col, _, _) = spanData sp
  [nodeId] <- lift $ lift $ lookupNameNode (pack file) start_row start_col

  let (d_file, d_start_row, d_start_col, d_end_row, d_end_col)
        = case nameSrcSpan name of RealSrcSpan rsp -> ( Just (unpackFS (srcSpanFile rsp)), Just (srcSpanStartLine rsp)
                                                      , Just (srcSpanStartCol rsp), Just (srcSpanEndLine rsp)
                                                      , Just (srcSpanEndCol rsp) )
                                   _               -> (Nothing, Nothing, Nothing, Nothing, Nothing)

      uniq = createNameUnique cm name
      namespace = occNameSpace $ nameOccName name
      nameStr = occNameString $ nameOccName name
  liftSelda $ insert_ names [ Just nodeId :*: scope :*: fmap pack d_file :*: d_start_row :*: d_start_col :*: d_end_row :*: d_end_col
                               :*: pack (showSDocUnsafe (pprNameSpace namespace)) :*: pack nameStr :*: pack uniq :*: defining ]

writeType :: SrcSpan -> Id -> TrfType ()
writeType sp id = do
  cm <- asks (ms_mod . compiledModule)
  let name = idName id
      (file, start_row, start_col, _, _) = spanData sp
      annot = showSDocUnsafe $ ppr $ idType id
      uniq = createNameUnique cm name
  liftSelda $ insert_ types [ pack uniq :*: pack annot ]

writeImplicitInfo :: (HsName n, HsHasName (FieldOcc n)) => (a -> [GHC.Name]) -> [HsRecField n a] -> TrfType ()
writeImplicitInfo select flds = doWriteImplicitInfo (map getLabelAndExpr flds)
  where getLabelAndExpr fld = ( getTheName $ hsGetNames (hsRecFieldLbl fld), getTheName $ select (hsRecFieldArg fld) )
        getTheName [] = error "writeImplicitInfo: missing names"
        getTheName (e:_) = e

doWriteImplicitInfo :: [(GHC.Name, GHC.Name)] -> TrfType ()
doWriteImplicitInfo bindings
  = do mod <- asks (ms_mod . compiledModule)
       liftSelda $ insert_ implicitBinds (map (\(n1,n2) -> pack (createNameUnique mod n1)
                                                             :*: pack (createNameUnique mod n2)) bindings)


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

createNameUnique :: GHC.Module -> GHC.Name -> String
createNameUnique mod name
  = maybe (showSDocUnsafe (pprModule mod) ++ "." ++ nameStr ++ "?" ++ show (nameUnique name))
          ((++ ("."++nameStr)) . showSDocUnsafe . pprModule)
          (nameModule_maybe name)
  where nameStr = occNameString $ nameOccName name
-------------------------------------------------------------------------

addToScope :: SrcSpan -> TrfType () -> TrfType ()
addToScope sp act = do
  stage <- asks exportStage
  if stage /= RenameStage then act else doAddToScope sp act

doAddToScope :: SrcSpan -> TrfType () -> TrfType ()
doAddToScope sp act = do
  let (file, start_row, start_col, end_row, end_col) = spanData sp
  newScope <- liftSelda $ insertWithPK scopes [ def :*: pack file :*: start_row :*: start_col :*: end_row :*: end_col ]
  local (\s -> s { scope = Just newScope }) act

scopedSequence :: (Exporter (Located e)) -> Exporter [Located e]
scopedSequence f (first:next:rest)
  = addToScope (combineLocated (next:rest)) (f first >> scopedSequence f (next:rest))
scopedSequence f elems = mapM_ f elems

--------------------------------------------------------------------------

export :: Schema c => c -> SrcSpan -> [(TrfType ())] -> TrfType ()
export ctor loc fields = do
  id <- writeInsert ctor loc
  mapM_ (\(i,val) -> goInto id i val) (zip [1..] fields)

exportError :: (Data s, Outputable s) => String -> s -> TrfType a
exportError exporter a =
  do stage <- asks exportStage
     liftIO $ throwIO $ ExportException stage (showSDocUnsafe $ ppr a) exporter (toConstr a)

data ExportException = ExportException ExportStage String String Constr deriving Show

instance Exception ExportException where
  displayException (ExportException st ppr exporter ctor)
    = "Unexpected element while exporting: " ++ ppr ++ "(" ++ show ctor ++ ") with: " ++ exporter ++ " in stage: " ++ show st

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

instance HsHasName GHC.Name where
  hsGetNames n = [n]

instance HsHasName Id where
  hsGetNames n = [idName n]

instance HsHasName (FieldOcc RdrName) where
  hsGetNames _ = []

instance HsHasName (FieldOcc GHC.Name) where
  hsGetNames (FieldOcc _ n) = [n]

instance HsHasName (FieldOcc Id) where
  hsGetNames (FieldOcc _ n) = [idName n]

instance HsHasName e => HsHasName [e] where
  hsGetNames es = concatMap hsGetNames es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames (L _ e) = hsGetNames e

instance HsHasName n => HsHasName (Pat n) where
  hsGetNames (VarPat id) = hsGetNames id

instance HsHasName n => HsHasName (HsBind n) where
  hsGetNames (FunBind {fun_id = lname}) = hsGetNames lname
