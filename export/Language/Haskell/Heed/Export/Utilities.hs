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
import qualified Outputable
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
import System.IO
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Writer
import Language.Haskell.Heed.TypeRepresentation
import Data.Maybe
import Data.List
import Data.Data hiding (typeRep)
import Data.Text (pack, unpack)

import Language.Haskell.Heed.Schema (Schema(..))
import qualified Language.Haskell.Heed.Schema as Schema

data ExportState = ExportState { parentData :: Maybe (Int, Int)
                               , exportStage :: ExportStage
                               , nameExportForced :: Bool
                               , compiledModule :: ModSummary
                               , isDefining :: Bool
                               , scopeToRegister :: Maybe SrcSpan -- used by definitions and inner scopes
                               , visibleScope :: Maybe SrcSpan -- cleared by recognizing element
                               , ambiguousNames :: [(SrcSpan, SrcSpan)]
                               , skippedRanges :: [SrcSpan]
                               , moduleRange :: SrcSpan
                               , defFileName :: String
                               , globalEnv :: Maybe TcGblEnv
                               , handles :: ExportHandles
                               , counters :: ExportCounters
                               }

data ExportStore = ExportStore [(SrcSpan, SrcSpan)] [SrcSpan]

withExportHandles :: MonadIO m => (ExportHandles -> m a) -> m a
withExportHandles action = do
  handles@[node] <- liftIO $ mapM (flip openFile AppendMode) [ "nodes.csv" ]
  res <- action (ExportHandles node)
  liftIO $ mapM_ hClose handles
  return res

data ExportHandles = ExportHandles { nodeHandle :: Handle }

data ExportCounters = ExportCounters { nodeCounter :: MVar Int }

emptyStore :: ExportStore
emptyStore = ExportStore [] []

instance Monoid ExportStore where
  mempty = emptyStore
  mappend (ExportStore an1 rngs1) (ExportStore an2 rngs2)
    = ExportStore (an1 ++ an2) (rngs1 ++ rngs2)

data ExportStage = ParsedStage | RenameStage | TypedStage deriving (Eq, Show)

initExportState :: ModSummary -> SrcSpan -> String -> ExportHandles -> ExportStore -> ExportStage
                     -> Maybe TcGblEnv -> IO ExportState
initExportState mod loc fileName handles (ExportStore amb skip) stage gblEnv
  = do nodeCounter <- newMVar 1
       let counters = ExportCounters nodeCounter
       return $ ExportState { parentData = Nothing
                            , exportStage = stage
                            , nameExportForced = False
                            , compiledModule = mod
                            , isDefining = False
                            , scopeToRegister = Nothing
                            , visibleScope = Nothing
                            , ambiguousNames = amb
                            , skippedRanges = skip
                            , moduleRange = loc
                            , defFileName = fileName
                            , globalEnv = gblEnv
                            , handles = handles
                            , counters = counters
                            }

type TrfType = ReaderT ExportState Ghc

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

goInto :: Maybe Int -> Int -> TrfType a -> TrfType a
goInto (Just ind) ref = local (\s -> s { parentData = Just (ind, ref) })
goInto Nothing _ = id

defining :: TrfType a -> TrfType a
defining = local $ \s -> s { isDefining = True }

notDefining :: TrfType a -> TrfType a
notDefining = local $ \s -> s { isDefining = False }

forceNameExport :: TrfType a -> TrfType a
forceNameExport = local $ \s -> s { nameExportForced = True }

writeInsert :: Schema s => s -> SrcSpan -> TrfType (Maybe Int)
writeInsert ctor loc = do
  stage <- asks exportStage
  if stage == ParsedStage then Just <$> writeInsert' ctor loc
                          else return Nothing

writeInsert' :: Schema s => s -> SrcSpan -> TrfType Int
writeInsert' ctor loc = do
  output <- asks (nodeHandle . handles)
  count <- asks (nodeCounter . counters)
  pd <- asks parentData
  let (file, startRow, startCol, endRow, endCol) = spanData loc

  ind <- liftIO $ modifyMVar count (\a -> return (a+1,a))

  liftIO
    $ hPutStrLn output
    $ -- key, type and constructor
      show ind ++ ";" ++ dataTypeName (dataTypeOf ctor) ++ ";" ++ show (toConstr ctor) ++ ";"
      -- location
        ++ file ++ ";" ++ show startRow ++ ";" ++ show startCol ++ ";" ++ show endRow ++ ";" ++ show endCol ++ ";"
      -- parent location and reference
        ++ (case pd of Just (ind,r) -> show ind ++ ";" ++ show r ++ ";"
                       _            -> ";;")
  return ind

writeIntAttribute :: Int -> TrfType ()
writeIntAttribute i = return ()

writeBoolAttribute :: Bool -> TrfType ()
writeBoolAttribute b = return ()

writeStringAttribute :: String -> TrfType ()
writeStringAttribute s = return ()

writeFractionalAttribute :: Double -> TrfType ()
writeFractionalAttribute f = return ()


writeModule :: ModSummary -> TrfType ()
writeModule ms = do
  return ()

writeName :: SrcSpan -> GHC.Name -> TrfType ()
writeName sp name = do
  sc <- asks visibleScope
  force <- asks nameExportForced
  when (force || isJust sc) $ doWriteName sp name sc

doWriteName :: SrcSpan -> GHC.Name -> Maybe SrcSpan -> TrfType ()
doWriteName sp name scope = do
  return ()

writeAmbiguousName :: SrcSpan -> TrfType ()
writeAmbiguousName loc
  = return ()

writeModImport :: HsName n => LImportDecl n -> TrfType ()
writeModImport (L l (ImportDecl _ n pkg _ _ qual _ declAs hiding)) = do
  return ()

writeImportedNames :: GHC.Module -> [(GHC.Name, Maybe GHC.Name)] -> Ghc ()
writeImportedNames mod importedNames =
  return ()

writeType :: SrcSpan -> Id -> TrfType ()
writeType sp id = do
  return ()

writeImplicitInfo :: (HsName n, HsHasName (FieldOcc n)) => (a -> [GHC.Name]) -> [HsRecField n a] -> TrfType ()
writeImplicitInfo select flds = doWriteImplicitInfo (map getLabelAndExpr flds)
  where getLabelAndExpr fld = ( getTheName $ hsGetNames (hsRecFieldLbl fld), getTheName $ select (hsRecFieldArg fld) )
        getTheName [] = error "writeImplicitInfo: missing names"
        getTheName (e:_) = e

doWriteImplicitInfo :: [(GHC.Name, GHC.Name)] -> TrfType ()
doWriteImplicitInfo bindings
  = return ()

writeCtors :: GHC.Name -> [GHC.Name] -> TrfType ()
writeCtors tn cns = do
  return ()

writeFields :: GHC.Name -> [GHC.Name] -> TrfType ()
writeFields cn fls = do
  return ()

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

-- * Handling and exporting scopes

newScope :: SrcSpan -> TrfType () -> TrfType () -> TrfType ()
newScope range population visible = do
  -- TODO: write out the range, and the range of the parent scope
  local (\s -> s { scopeToRegister = Just range }) population
  local (\s -> s { scopeToRegister = Just range, visibleScope = Just range }) visible

newScope_ :: SrcSpan -> TrfType () -> TrfType ()
newScope_ sp = newScope sp (return ())

scopedSequence :: (Exporter (Located e)) -> Exporter [Located e]
scopedSequence f (first:next:rest)
  = newScope (combineLocated (next:rest)) (f first) (scopedSequence f (next:rest))
scopedSequence f elems = mapM_ f elems

--------------------------------------------------------------------------

export :: Schema c => c -> SrcSpan -> [(TrfType ())] -> TrfType ()
export ctor loc fields = do
  skipped <- asks skippedRanges -- skip elements that are generated by TH
  when (all (not . (`spanCont` loc)) skipped) $ do
    ind <- writeInsert ctor loc
    mapM_ (\(i,val) -> goInto ind i val) (zip [1..] fields)

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
    env <- lift getSession
    Just gbl <- asks globalEnv
    tcSpl <- liftIO $ runTcInteractive env { hsc_dflags = xopt_set (hsc_dflags env) TemplateHaskellQuotes }
      $ updGblEnv (const gbl)
      $ (L rng <$> rename rdr)
    let typecheckErrors = showSDocUnsafe (vcat (pprErrMsgBagWithLoc (fst (fst tcSpl)))
                                            <+> vcat (pprErrMsgBagWithLoc (snd (fst tcSpl))))
    return $ fromMaybe (error $ "runRenamer: " ++ show rng ++ " " ++ typecheckErrors)
                       (snd tcSpl)

