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

type Node = RowID :*: Maybe RowID :*: Int :*: Int :*: Text :*: Int :*: Int :*: Int :*: Int :*: Maybe Int

nodes :: Table Node
nodes = table "nodes" $ autoPrimary "node_id"
                          :*: optional "parent_id" -- `fk` (nodes, node_id)
                          :*: required "node_type"
                          :*: required "ctor"
                          :*: required "file"
                          :*: required "start_row"
                          :*: required "start_col"
                          :*: required "end_row"
                          :*: required "end_col"
                          :*: optional "parent_handle"
( node_id
    :*: node_parent
    :*: node_type
    :*: node_ctor
    :*: node_file
    :*: node_start_row
    :*: node_start_col
    :*: node_end_row
    :*: node_end_col
    :*: parent_handle ) = selectors nodes

type Attribute = RowID :*: Int :*: Maybe Text :*: Maybe Int :*: Maybe Double

attributes :: Table Attribute
attributes = table "attributes" $ required "container"
                                    :*: required "attribute_handle"
                                    :*: optional "text_attribute"
                                    :*: optional "integer_attribute"
                                    :*: optional "fractional_attribute"
( container
    :*: attribute_handle
    :*: text_attribute
    :*: integer_attribute
    :*: fractional_attribute ) = selectors attributes

type Module = RowID :*: Text :*: Text :*: Text

modules :: Table Module
modules = table "modules" $ autoPrimary "module_id"
                              :*: required "module_name"
                              :*: required "package_name"
                              :*: required "module_source"
(module_id :*: module_name :*: module_package :*: module_source) = selectors modules

-- * Semantic information

type Name = Maybe RowID :*: RowID :*: Maybe Text :*: Maybe Int :*: Maybe Int :*: Maybe Int :*: Maybe Int
              :*: Text :*: Text :*: Text :*: Bool

names :: Table Name
names = table "names" $ optional "name_node" -- `fk` (nodes, node_id)
                          :*: required "name_scope" `fk` (scopes, scope_id) -- the scope in which the name is bound
                          :*: optional "def_file"
                          :*: optional "def_start_row"
                          :*: optional "def_start_col"
                          :*: optional "def_end_row"
                          :*: optional "def_end_col"
                          :*: required "name_namespace"
                          :*: required "name_str"
                          :*: required "name_uniq"
                          :*: required "name_defining"
( name_node
    :*: name_scope
    :*: def_file
    :*: def_start_row
    :*: def_start_col
    :*: def_end_row
    :*: def_end_col
    :*: name_namespace
    :*: name_str
    :*: name_uniq
    :*: name_defining ) = selectors names

type Type = Text :*: Text

types :: Table Type
types = table "types" $ required "type_name" -- `fk` (names, name_uniq)
                          :*: required "type_desc"
( type_name :*: type_desc ) = selectors types

type ImplicitBinds = Text :*: Text

implicitBinds :: Table ImplicitBinds
implicitBinds = table "implicit_binds" $ required "imp_bind_lhs"
                                           :*: required "imp_bind_rhs"
( imp_bind_lhs :*: imp_bind_rhs ) = selectors implicitBinds

type CtorField = Text :*: Text

ctorFields :: Table CtorField
ctorFields = table "ctor_fields" $ required "cf_constructor" :*: required "cf_field"
( cf_constructor :*: cf_field ) = selectors ctorFields

type TypeCtor = Text :*: Text

typeCtors :: Table TypeCtor
typeCtors = table "type_ctors" $ required "tc_type" :*: required "tc_ctor"
( ct_type :*: ct_ctor ) = selectors typeCtors

type Scope = RowID :*: Text :*: Int :*: Int :*: Int :*: Int

scopes :: Table Scope
scopes = table "scopes" $ autoPrimary "scope_id"
                           :*: required "file"
                           :*: required "start_row"
                           :*: required "start_col"
                           :*: required "end_row"
                           :*: required "end_col"
( scope_id
    :*: scope_file
    :*: scope_start_row
    :*: scope_start_col
    :*: scope_end_row
    :*: scope_end_col ) = selectors scopes

-- * Lexical information

type Token = Text :*: Int :*: Int :*: Int :*: Int :*: Text

tokens :: Table Token
tokens = table "tokens" $ required "file"
                            :*: required "start_row"
                            :*: required "start_col"
                            :*: required "end_row"
                            :*: required "end_col"
                            :*: required "token"
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
