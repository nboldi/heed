{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Core where

import Control.Monad
import Control.Exception (throwIO)
import Control.Monad.Trans.Class
import Data.Data (Data(..))
import Data.List
import Data.List.Split
import Data.Maybe

import Data.Text (pack)

import Database.Selda

import Control.Monad.Catch
import GHC (gcatch)
import GhcMonad

import qualified Database.Selda.Backend as Backend -- just for disabling foreign keys to drop tables

instance MonadThrow Ghc where
    throwM  = liftIO . throwIO

instance MonadCatch Ghc where
    catch   = GHC.gcatch

instance MonadMask Ghc where
    mask f = wrap $ \s ->
               mask $ \io_restore ->
                 unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
     where
        wrap   = GhcMonad.Ghc
        unwrap = GhcMonad.unGhc
    uninterruptibleMask = mask

liftGhc :: Ghc a -> SeldaT Ghc a
liftGhc = lift

withForeignCheckTurnedOff :: (MonadMask m, MonadIO m, MonadThrow m) => SeldaT m a -> SeldaT m a
withForeignCheckTurnedOff act = do
   backend <- Backend.seldaBackend
   (resCode, _) <- liftIO $ Backend.runStmt backend (pack "PRAGMA foreign_keys = OFF") [] -- TODO: something different for postgre
   when (resCode /= 0) (throwM $ Backend.SqlError "withForeignCheckTurnedOff: Switching foreign keys off was not successful.")
   res <- act
   (resCode, _) <- liftIO $ Backend.runStmt backend (pack "PRAGMA foreign_keys = ON") []
   when (resCode /= 0) (throwM $ Backend.SqlError "withForeignCheckTurnedOff: Switching foreign keys back on was not successful.")
   return res

-- * Syntax

type Node = RowID :*: Maybe RowID :*: Int :*: Int :*: Text :*: Int :*: Int :*: Int :*: Int :*: Maybe Int :*: RowID

nodes :: Table Node
nodes = table "nodes" $ autoPrimary "node_id"
                          :*: optional "parent_id" `optFk` (nodes, node_id)
                          :*: required "node_type"
                          :*: required "ctor"
                          :*: required "file"
                          :*: required "start_row"
                          :*: required "start_col"
                          :*: required "end_row"
                          :*: required "end_col"
                          :*: optional "parent_handle"
                          :*: required "node_module"
( node_id
    :*: node_parent
    :*: node_type
    :*: node_ctor
    :*: node_file
    :*: node_start_row
    :*: node_start_col
    :*: node_end_row
    :*: node_end_col
    :*: parent_handle
    :*: node_module ) = selectors nodes

type Attribute = RowID :*: Int :*: RowID :*: Maybe Text :*: Maybe Int :*: Maybe Double

attributes :: Table Attribute
attributes = table "attributes" $ required "container" `fk` (nodes, node_id)
                                    :*: required "attribute_handle"
                                    :*: required "attribute_module" `fk` (modules, module_id)
                                    :*: optional "text_attribute"
                                    :*: optional "integer_attribute"
                                    :*: optional "fractional_attribute"
( container
    :*: attribute_handle
    :*: attribute_module
    :*: text_attribute
    :*: integer_attribute
    :*: fractional_attribute ) = selectors attributes

type Module = RowID :*: Text :*: Text :*: Maybe Text :*: Maybe Text

modules :: Table Module
modules = table "modules" $ autoPrimary "module_id"
                              :*: required "module_name"
                              :*: required "package_name"
                              :*: optional "module_source"
                              :*: optional "module_hash"
(module_id :*: module_name :*: module_package :*: module_source :*: module_hash) = selectors modules

-- * Semantic information

type Name = RowID :*: Text :*: Text :*: Text :*: Bool :*: RowID

names :: Table Name
names = table "names" $ required "name_node" `fk` (nodes, node_id)
                          :*: required "name_namespace"
                          :*: required "name_str"
                          :*: required "name_uniq"
                          :*: required "name_defining"
                          :*: required "name_module" `fk` (modules, module_id)
( name_node
    :*: name_namespace
    :*: name_str
    :*: name_uniq
    :*: name_defining
    :*: name_module ) = selectors names

type Definition = Maybe RowID :*: Maybe RowID :*: Text :*: Text :*: Text :*: Maybe Text

definitions :: Table Definition
definitions = table "definitions" $ optional "def_module" `optFk` (modules, module_id)
                                      :*: optional "def_scope" `optFk` (scopes, scope_id)
                                      :*: required "def_namespace"
                                      :*: required "def_str"
                                      :*: required "def_uniq"
                                      :*: optional "def_parent_uniq"
( def_module
    :*: def_scope
    :*: def_namespace
    :*: def_str
    :*: def_uniq
    :*: def_parent_uniq ) = selectors definitions

type Type = Text :*: Text :*: RowID

types :: Table Type
types = table "types" $ required "type_name"
                          :*: required "type_desc"
                          :*: required "type_module" `fk` (modules, module_id)
( type_name :*: type_desc :*: type_module ) = selectors types

type ImplicitBinds = Text :*: Text :*: RowID

implicitBinds :: Table ImplicitBinds
implicitBinds = table "implicit_binds" $ required "imp_bind_lhs"
                                           :*: required "imp_bind_rhs"
                                           :*: required "imp_bind_module" `fk` (modules, module_id)
( imp_bind_lhs :*: imp_bind_rhs :*: imp_bind_module ) = selectors implicitBinds

type CtorField = Text :*: Text :*: RowID

ctorFields :: Table CtorField
ctorFields = table "ctor_fields" $ required "cf_constructor"
                                     :*: required "cf_field"
                                     :*: required "cf_module" `fk` (modules, module_id)
( cf_constructor :*: cf_field :*: cf_module ) = selectors ctorFields
 -- `fk` (modules, module_id)

type TypeCtor = Text :*: Text :*: RowID

typeCtors :: Table TypeCtor
typeCtors = table "type_ctors" $ required "tc_type"
                                   :*: required "tc_ctor"
                                   :*: required "tc_module" `fk` (modules, module_id)
( ct_type :*: ct_ctor :*: ct_module ) = selectors typeCtors

type Scope = RowID :*: Text :*: Int :*: Int :*: Int :*: Int

scopes :: Table Scope
scopes = table "scopes" $ autoPrimary "scope_id"
                           :*: required "scope_file"
                           :*: required "scope_start_row"
                           :*: required "scope_start_col"
                           :*: required "scope_end_row"
                           :*: required "scope_end_col"
( scope_id
    :*: scope_file
    :*: scope_start_row
    :*: scope_start_col
    :*: scope_end_row
    :*: scope_end_col ) = selectors scopes

type ModuleImport = RowID :*: RowID :*: Bool :*: Bool :*: Text :*: Maybe RowID :*: RowID

moduleImports :: Table ModuleImport
moduleImports = table "module_imports" $ required "mi_scope_id" `fk` (scopes, scope_id)
                                          :*: required "mi_module_id" `fk` (modules, module_id)
                                          :*: required "mi_qualified"
                                          :*: required "mi_just_listed"
                                          :*: required "mi_qualifier"
                                          :*: optional "mi_node" `optFk` (nodes, node_id)
                                          :*: required "mi_contain_module" `fk` (modules, module_id)
( mi_scope_id
    :*: mi_module_id
    :*: mi_qualified
    :*: mi_just_listed
    :*: mi_qualifier
    :*: mi_node
    :*: mi_contain_module ) = selectors moduleImports

type ModuleImportMod = RowID :*: Text :*: Text :*: RowID

moduleImportShowing :: Table ModuleImportMod
moduleImportShowing = table "module_imports_showing" $ required "mis_node"
                                                         :*: required "mis_name"
                                                         :*: required "mis_str"
                                                         :*: required "mis_module" `fk` (modules, module_id)
( mis_node :*: mis_name :*: mis_str :*: mis_module ) = selectors moduleImportShowing

moduleImportHiding :: Table ModuleImportMod
moduleImportHiding = table "module_imports_hiding" $ required "mih_node"
                                                       :*: required "mih_name"
                                                       :*: required "mih_str"
                                                       :*: required "mih_module" `fk` (modules, module_id)
( mih_node :*: mih_name :*: mih_str :*: mih_module ) = selectors moduleImportHiding

-- * Lexical information

type Token = Text :*: Int :*: Int :*: Int :*: Int :*: Text

tokens :: Table Token
tokens = table "tokens" $ required "file"
                            :*: required "start_row"
                            :*: required "start_col"
                            :*: required "end_row"
                            :*: required "end_col"
                            :*: required "token_str"
( token_file
    :*: token_start_row
    :*: token_start_col
    :*: token_end_row
    :*: token_end_col
    :*: token_str ) = selectors tokens

type Comment = Text :*: Int :*: Int :*: Int :*: Int :*: Text :*: Text

comments :: Table Comment
comments = table "comments" $ required "file"
                                :*: required "start_row"
                                :*: required "start_col"
                                :*: required "end_row"
                                :*: required "end_col"
                                :*: required "type"
                                :*: required "content"
( comment_file
    :*: comment_start_row
    :*: comment_start_col
    :*: comment_end_row
    :*: comment_end_col
    :*: comment_type
    :*: comment_str ) = selectors comments

data TypeRepresentation = TRForAll [String] TypeRepresentation
                        | TRVar String
                        | TRFun [TypeRepresentation] TypeRepresentation
                        | TRApp TypeRepresentation TypeRepresentation
                        | TRConApp String [TypeRepresentation]
                        | TRNumLit Integer
                        | TRStringLit String
                        | TRCoercion
  deriving (Show, Read)

typeRepEq :: TypeRepresentation -> TypeRepresentation -> Bool
typeRepEq = compareTypeRep []

compareTypeRep :: [(String,String)] -> TypeRepresentation -> TypeRepresentation -> Bool
compareTypeRep env (TRForAll vars1 t1) (TRForAll vars2 t2)
  = compareTypeRep (env ++ zip vars1 vars2) t1 t2
compareTypeRep env (TRVar v1) (TRVar v2)
  = case find ((==v1) . fst) env of Just (v1',v2') -> v2 == v2'
                                    Nothing -> isNothing (find ((==v2) . snd) env) && v1 == v2
compareTypeRep env (TRFun args1 res1) (TRFun args2 res2)
  = and (zipWith (compareTypeRep env) args1 args2) && compareTypeRep env res1 res2
compareTypeRep env (TRApp base1 arg1) (TRApp base2 arg2)
  = compareTypeRep env base1 base2 && compareTypeRep env arg1 arg2
compareTypeRep env (TRConApp base1 args1) (TRConApp base2 args2)
  = base1 == base2 && and (zipWith (compareTypeRep env) args1 args2)
compareTypeRep _ (TRNumLit i1) (TRNumLit i2) = i1 == i2
compareTypeRep _ (TRStringLit s1) (TRStringLit s2) = s1 == s2
compareTypeRep _ TRCoercion TRCoercion = True
compareTypeRep _ _ _ = False

tabulate :: Monad m => Int -> ([a] -> [a] -> [a]) -> [b] -> ([b] -> m [a]) -> m [a]
tabulate n comb inp op = foldl1 comb <$> mapM op chunks
  where chunks = chunksOf n inp
