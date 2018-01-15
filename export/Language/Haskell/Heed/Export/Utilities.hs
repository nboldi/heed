{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
module Language.Haskell.Heed.Export.Utilities where

import GHC
import FastString
import Outputable hiding (text, int)
import Name
import Id
import SrcLoc
import Outputable (Outputable(..), OutputableBndr)
import Bag
import HscTypes
import Type
import BasicTypes
import RnSplice
import TcRnTypes
import RnEnv
import RnExpr
import ErrUtils
import TcRnMonad
import DynFlags
import Avail
import Language.Haskell.TH.LanguageExtensions

import System.Directory
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Writer
import Database.Selda
import Core
import Data.Maybe
import Data.List
import Data.Data hiding (typeRep)
import Data.Text (pack)

import Language.Haskell.Heed.Schema (Schema(..))
import qualified Language.Haskell.Heed.Schema as Schema

data ExportState = ExportState { parentData :: Maybe (RowID, Int)
                               , exportStage :: ExportStage
                               , nameExportForced :: Bool
                               , compiledModule :: ModSummary
                               , isDefining :: Bool
                               , scope :: Maybe RowID
                               , ambiguousNames :: [(SrcSpan, RowID)]
                               , skippedRanges :: [SrcSpan]
                               , moduleRange :: SrcSpan
                               , globalEnv :: Maybe TcGblEnv
                               }

data ExportStore = ExportStore [(SrcSpan, RowID)] [SrcSpan]

emptyStore :: ExportStore
emptyStore = ExportStore [] []

instance Monoid ExportStore where
  mempty = emptyStore
  mappend (ExportStore an1 rngs1) (ExportStore an2 rngs2)
    = ExportStore (an1 ++ an2) (rngs1 ++ rngs2)

data ExportStage = ParsedStage | RenameStage | TypedStage deriving (Eq, Show)

liftSelda :: SeldaT Ghc a -> TrfType a
liftSelda = lift . lift

initExportState :: ModSummary -> SrcSpan -> ExportStore -> ExportStage -> Maybe TcGblEnv -> ExportState
initExportState mod loc (ExportStore amb skip) stage gblEnv
  = ExportState Nothing stage False mod False Nothing amb skip loc gblEnv

type TrfType = ReaderT ExportState (WriterT ExportStore (SeldaT Ghc))

type Exporter n = n -> TrfType ()

class IsRdrName n where
  toRdrName :: n -> RdrName

class (OutputableBndr n, OutputableBndr (NameOrRdrName n), HsHasName (FieldOcc n)) => CompilationPhase n where
  getBindsAndSigs :: HsValBinds n -> ([LSig n], LHsBinds n)
  renameSplice :: Located (HsSplice n) -> TrfType (Maybe (Located (HsSplice GHC.Name)))
  renameBracket :: Located (HsBracket n) -> TrfType (Maybe (Located (HsBracket GHC.Name)))
  renameRole :: Located (HsDecl n) -> TrfType (Maybe (Located (HsDecl GHC.Name)))

instance CompilationPhase RdrName where
  getBindsAndSigs (ValBindsIn binds sigs) = (sigs, binds)
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsOut in parsed source"
  renameSplice = fmap Just . rdrSplice
  renameBracket = fmap Just . rdrBracket
  renameRole = fmap Just . rdrRole

instance CompilationPhase GHC.Name where
  getBindsAndSigs (ValBindsOut bindGroups sigs) = (sigs, unionManyBags (map snd bindGroups))
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsIn in renamed source"
  renameSplice _ = return Nothing
  renameBracket _ = return Nothing
  renameRole _ = return Nothing

instance CompilationPhase Id where
  getBindsAndSigs (ValBindsOut bindGroups _) = ([], unionManyBags (map snd bindGroups))
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsIn in renamed source"
  renameSplice _ = return Nothing
  renameBracket _ = return Nothing
  renameRole _ = return Nothing

class (DataId n, HasOccName n, HsHasName n, IsRdrName n, Outputable n, CompilationPhase n)
      => HsName n where
  exportName :: Exporter (Located n)
  exportOperator :: Exporter (Located n)
  exportNameOrRdrName :: (forall x . HsName x => Exporter (Located x)) -> Exporter (Located (NameOrRdrName n))
  exportFieldOccName :: Exporter (Located (FieldOcc n))
  exportAmbiguous :: (forall x . HsName x => Exporter (Located x)) -> Exporter (Located (AmbiguousFieldOcc n))

goInto :: Maybe RowID -> Int -> TrfType a -> TrfType a
goInto (Just id) ref = local (\s -> s { parentData = Just (id, ref) })
goInto Nothing _ = id

defining :: TrfType a -> TrfType a
defining = local $ \s -> s { isDefining = True }

notDefining :: TrfType a -> TrfType a
notDefining = local $ \s -> s { isDefining = False }

forceNameExport :: TrfType a -> TrfType a
forceNameExport = local $ \s -> s { nameExportForced = True }

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
lookupNameNode = lookupNode [ typeId (undefined :: Schema.Name), typeId (undefined :: Schema.Operator) ]

lookupNode :: [Int] -> Text -> Int -> Int -> SeldaT Ghc [RowID]
lookupNode types = prepared $ \file start_row start_col -> do
  n <- select nodes
  restrict $ file .== n ! node_file
              .&& start_row .== n ! node_start_row
              .&& start_col .== n ! node_start_col
              .&& n ! node_type `isIn` (map int types)
  return (n ! node_id)

lookupImportedModule :: Text -> Text -> SeldaT Ghc [RowID :*: Maybe Text]
lookupImportedModule = prepared $ \mod pkg -> do
  (id :*: name :*: package :*: _ :*: hash) <- select modules
  restrict $ mod .== name .&& pkg .== package
  return (id :*: hash)

lookupOneModule :: Text -> Text -> SeldaT Ghc (RowID :*: Maybe Text)
lookupOneModule mod pkg = do res <- lookupImportedModule mod pkg
                             case res of [m] -> return m
                                         _ -> error $ "lookupOneModule: " ++ show mod ++ " " ++ show pkg

writeModule :: TrfType ()
writeModule = do
  ms <- asks compiledModule
  let mod = ms_mod ms
  sourcePath <- liftIO $ makeAbsolute (msHsFilePath ms)
  liftSelda $ insert_ modules [ def :*: pack (moduleNameString $ moduleName mod)
                                    :*: pack (show $ moduleUnitId mod)
                                    :*: Just (pack sourcePath)
                                    :*: Nothing ]

writeName :: SrcSpan -> GHC.Name -> TrfType ()
writeName sp name = do
  sc <- asks scope
  force <- asks nameExportForced
  when (force || isJust sc) $ doWriteName sp name sc

doWriteName :: SrcSpan -> GHC.Name -> Maybe RowID -> TrfType ()
doWriteName sp name scope = do
  cm <- asks (ms_mod . compiledModule)
  defining <- asks isDefining
  st <- asks exportStage
  let (file, start_row, start_col, _, _) = spanData sp
  nodeIds <- liftSelda $ lookupNameNode (pack file) start_row start_col
  case nodeIds of
    [] -> liftIO $ putStrLn $ "WARNING: no node id is found at: " ++ show st ++ " " ++ show sp ++ " writing out " ++ showSDocUnsafe (ppr name)
    _:_:_ -> liftIO $ putStrLn $ "WARNING: multiple node ids found at: " ++ show st ++ " " ++ show sp ++ " writing out " ++ showSDocUnsafe (ppr name)
    [nodeId] -> do
      let uniq = createNameUnique cm name
          namespace = occNameSpace $ nameOccName name
          nameStr = occNameString $ nameOccName name
          nsRecord = showSDocUnsafe (pprNameSpace namespace)
          mod = case nameModule_maybe name of Just nm -> moduleNameString (moduleName nm)
                                              Nothing -> moduleNameString (moduleName cm)
      when defining
        $ liftSelda $ insert_ definitions [ Nothing :*: scope :*: pack nsRecord :*: pack nameStr :*: pack uniq :*: Nothing ]
      liftSelda $ insert_ names [ nodeId :*: pack nsRecord :*: pack nameStr :*: pack uniq :*: defining ]

writeModImport :: HsName n => LImportDecl n -> TrfType ()
writeModImport (L l (ImportDecl _ n pkg _ _ qual _ declAs hiding)) = do
  sc <- asks scope
  cm <- asks (ms_mod . compiledModule)
  mod <- liftSelda $ liftGhc $ findModule (unLoc n) (fmap sl_fs pkg)
  modIds <- liftSelda $ lookupImportedModule (pack $ moduleNameString $ moduleName mod) (pack $ show $ moduleUnitId mod)
  let (file, start_row, start_col, _, _) = spanData l
  importNode <- liftSelda $ lookupNode [ typeId (undefined :: Schema.ImportDecl) ] (pack file) start_row start_col
  let trfLie n ie = n :*: pack (createNameUnique cm name) :*: pack (occNameString (occName name))
        where name = head . hsGetNames . ieName . unLoc $ ie
  case (importNode, hiding) of
    ([n], Just (True, h)) -> liftSelda $ insert_ moduleImportHiding (map (trfLie n) (unLoc h))
    ([n], Just (False, h)) -> liftSelda $ insert_ moduleImportShowing (map (trfLie n) (unLoc h))
    _ -> return ()
  case (sc, map first modIds) of
    (Just sc, [modId])
      -> liftSelda $ insert_ moduleImports
           [ sc :*: modId :*: qual :*: maybe False (not . fst) hiding
                :*: pack (moduleNameString (maybe (unLoc n) unLoc declAs))
                :*: listToMaybe importNode ]
    _ -> return ()

writeImportedNames :: GHC.Module -> RowID -> [(GHC.Name, Maybe GHC.Name)] -> SeldaT Ghc ()
writeImportedNames mod modId importedNames = do
  insert_ definitions (map (createNameImport modId) importedNames)
  where createNameImport modId (name, parent)
          = Just modId :*: Nothing :*: pack (showSDocUnsafe (pprNameSpace (namespace name)))
               :*: pack (nameStr name) :*: pack (uniq name) :*: fmap (pack . uniq) parent
          where uniq n = createNameUnique mod n
                namespace n = occNameSpace $ nameOccName n
                nameStr n = occNameString $ nameOccName n

writeType :: SrcSpan -> Id -> TrfType ()
writeType sp id = do
  cm <- asks (ms_mod . compiledModule)
  let name = idName id
      (file, start_row, start_col, _, _) = spanData sp
      uniq = createNameUnique cm name
  let typeRep = toTypeRep cm (idType id)
  liftSelda $ insert_ types [ pack uniq :*: pack (show typeRep) ]

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

writeCtors :: GHC.Name -> [GHC.Name] -> TrfType ()
writeCtors tn cns = do
  mod <- asks (ms_mod . compiledModule)
  liftSelda $ insert_ typeCtors (map (\cn -> pack (createNameUnique mod tn) :*: pack (createNameUnique mod cn)) cns)

writeFields :: GHC.Name -> [GHC.Name] -> TrfType ()
writeFields cn fls = do
  mod <- asks (ms_mod . compiledModule)
  liftSelda $ insert_ ctorFields (map (\fl -> pack (createNameUnique mod cn) :*: pack (createNameUnique mod fl)) fls)

toTypeRep :: GHC.Module -> GHC.Type -> TypeRepresentation
toTypeRep mod t
  | Just tv <- getTyVar_maybe t
  = TRVar (createNameUnique mod (getName tv))
  | (args, res) <- splitFunTys t, not (null args)
  = TRFun (map (toTypeRep mod) args) (toTypeRep mod res)
  | Just (base, arg) <- splitAppTy_maybe t
  = TRApp (toTypeRep mod base) (toTypeRep mod arg)
  | Just (tc, args) <- splitTyConApp_maybe t
  = TRConApp (createNameUnique mod (getName tc)) (map (toTypeRep mod) args)
  | (tvs, t') <- splitForAllTys t, not (null tvs)
  = TRForAll (map (createNameUnique mod . getName) tvs) (toTypeRep mod t')
  | Just i <- isNumLitTy t
  = TRNumLit i
  | Just str <- isStrLitTy t
  = TRStringLit (unpackFS str)
  | Just (t',_) <- splitCastTy_maybe t
  = toTypeRep mod t'
  | isCoercionType t
  = TRCoercion

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
  skipped <- asks skippedRanges -- skip elements that are generated by TH
  when (all (not . (`spanCont` loc)) skipped) $ do
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

spanCont :: SrcSpan -> SrcSpan -> Bool
spanCont (RealSrcSpan sp1) (RealSrcSpan sp2) = sp1 `containsSpan` sp2
spanCont _ _ = False

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

instance HsHasName e => HsHasName (Located e) where
  hsGetNames (L _ e) = hsGetNames e

-- * Renaming things that are missing from later representations

rdrSplice :: Located (HsSplice RdrName) -> TrfType (Located (HsSplice GHC.Name))
rdrSplice = runRenamer tcHsSplice'
  where
    tcHsSplice' (HsTypedSplice dec id e)
      = HsTypedSplice dec (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsUntypedSplice dec id e)
      = HsUntypedSplice dec (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsQuasiQuote id1 id2 sp fs)
      = pure $ HsQuasiQuote (mkUnboundNameRdr id1) (mkUnboundNameRdr id2) sp fs

rdrBracket :: Located (HsBracket RdrName) -> TrfType (Located (HsBracket GHC.Name))
rdrBracket = runRenamer (\br -> extractBr . fst <$> rnBracket undefined br)
  where extractBr (HsBracket br) = br
        extractBr (HsRnBracketOut br _) = br

rdrRole :: Located (HsDecl RdrName) -> TrfType (Located (HsDecl GHC.Name))
rdrRole = runRenamer tcHsRole
  where tcHsRole (RoleAnnotD (RoleAnnotDecl (L l name) roles))
          = RoleAnnotD <$> (RoleAnnotDecl <$> (L l <$> lookupOccRn name) <*> pure roles)

runRenamer :: Outputable (a GHC.Name) => (a RdrName -> RnM (a GHC.Name)) -> Located (a RdrName) -> TrfType (Located (a GHC.Name))
runRenamer rename (L rng rdr) = do
    env <- liftSelda $ liftGhc getSession
    Just gbl <- asks globalEnv
    tcSpl <- liftIO $ runTcInteractive env { hsc_dflags = xopt_set (hsc_dflags env) TemplateHaskellQuotes }
      $ updGblEnv (const gbl)
      $ (L rng <$> rename rdr)
    let typecheckErrors = showSDocUnsafe (vcat (pprErrMsgBagWithLoc (fst (fst tcSpl)))
                                            <+> vcat (pprErrMsgBagWithLoc (snd (fst tcSpl))))
    return $ fromMaybe (error $ "runRenamer: " ++ show rng ++ " " ++ typecheckErrors)
                       (snd tcSpl)

