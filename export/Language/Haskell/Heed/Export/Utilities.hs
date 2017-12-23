{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.Heed.Export.Utilities where

import GHC
import FastString
import Outputable (showSDocUnsafe, ppr)
import Name
import SrcLoc

import Control.Exception
import Control.Monad.Reader
import Database.Selda
import Core
import Data.Data
import Data.Text (pack)

data ExportState = ExportState { parentData :: Maybe (RowID, Int)
                               , exportSyntax :: Bool
                               , compiledModule :: Module
                               , isDefining :: Bool
                               , scope :: RowID
                               }

initExportState :: Bool -> Module -> ExportState
initExportState exportSyntax mod = ExportState Nothing exportSyntax mod False undefined

type TrfType = ReaderT ExportState (SeldaT Ghc)

class (DataId n, HasOccName n, HsHasName n) => HsName n where
  exportName :: Located n -> TrfType ()
  exportNameOrRdrName :: Located (NameOrRdrName n) -> TrfType ()
  exportFieldOccName :: Located (FieldOcc n) -> TrfType ()


goInto :: Maybe RowID -> Int -> TrfType a -> TrfType a
goInto (Just id) ref = local (\s -> s { parentData = Just (id, ref) })
goInto Nothing _ = id

defining :: TrfType a -> TrfType a
defining = local $ \s -> s { isDefining = True }

writeInsert :: Data s => Node -> s -> SrcSpan -> TrfType (Maybe RowID)
writeInsert typ ctor loc = do
  expSyntax <- asks exportSyntax
  if expSyntax then Just <$> writeInsert' typ ctor loc
               else return Nothing

writeInsert' :: Data s => Node -> s -> SrcSpan -> TrfType RowID
writeInsert' typ ctor loc = do
  parentRef <- asks parentData
  let (file, start_row, start_col, end_row, end_col) = spanData loc
  lift $ insertWithPK nodes [ def :*: fmap fst parentRef :*: constrIndex (toConstr typ)
                                :*: constrIndex (toConstr ctor) :*: pack file
                                :*: start_row :*: start_col :*: end_row :*: end_col
                                :*: fmap snd parentRef ]



lookupNameNode :: Text -> Int -> Int -> SeldaT Ghc [RowID]
lookupNameNode = prepared $ \file start_row start_col -> do
  n <- select nodes
  restrict $ file .== n ! node_file
              .&& start_row .== n ! node_start_row
              .&& start_col .== n ! node_start_col
              .&& int (constrIndex $ toConstr Name) .== n ! node_type
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

spanData :: SrcSpan -> (String, Int, Int, Int, Int)
spanData sp = case sp of RealSrcSpan rsp -> ( unpackFS (srcSpanFile rsp)
                                            , srcSpanStartLine rsp
                                            , srcSpanStartCol rsp
                                            , srcSpanEndLine rsp
                                            , srcSpanEndCol rsp )
                         _ -> ("", 0, 0, 0, 0)

combineLocated :: [Located a] -> SrcSpan
combineLocated = foldl combineSrcSpans noSrcSpan . map getLoc

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

--------------------------------------------------------------------------

data Node = Module
          | Declaration
          | Binding
          | KindSignature
          | Kind
          | Type
          | PatternField
          | Pattern
          | Expression
          | Splice
          | QuasiQuotation
          | Name
          | Operator
          | Literal
          | Rhs
          | Match
  deriving Data

export :: Data c => Node -> c -> SrcSpan -> [(TrfType ())] -> TrfType ()
export typ ctor loc fields = do
  id <- writeInsert typ ctor loc
  mapM_ (\(i,val) -> goInto id i val) (zip [1..] fields)

exportError :: Data s => s -> a
exportError a = throw $ ExportException (dataTypeOf a) (toConstr a)

data ExportException = ExportException DataType Constr deriving Show

instance Exception ExportException where
  displayException (ExportException typ ctor)
    = "Unexpected element while exporting: " ++ show ctor ++ " of type " ++ show typ

--------------------------------------------------------------------------


class HsHasName a where
  hsGetNames :: a -> [GHC.Name]

instance HsHasName RdrName where
  hsGetNames _ = []

instance HsHasName Name where
  hsGetNames n = [n]

instance HsHasName e => HsHasName [e] where
  hsGetNames es = concatMap hsGetNames es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames (L _ e) = hsGetNames e

instance HsHasName n => HsHasName (Pat n) where
  hsGetNames (VarPat id) = hsGetNames id

instance HsHasName n => HsHasName (HsBind n) where
  hsGetNames (FunBind {fun_id = lname}) = hsGetNames lname
