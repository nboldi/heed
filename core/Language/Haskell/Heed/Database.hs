{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Heed.Database where

import Database.Selda

-- * Syntax

type Node = RowID :*: Maybe RowID :*: Int :*: Int :*: RowID :*: Int :*: Int :*: Int :*: Int :*: Maybe Int :*: RowID

nodes :: Table Node
nodes = table "nodes" $ autoPrimary "node_id"
                          :*: optional "parent_id" `optFk` (nodes, node_id)
                          :*: required "node_type"
                          :*: required "node_ctor"
                          :*: required "node_file" `fk` (files, file_id)
                          :*: required "start_row"
                          :*: required "start_col"
                          :*: required "end_row"
                          :*: required "end_col"
                          :*: optional "parent_handle"
                          :*: required "node_module" `fk` (modules, module_id)
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

type File = RowID :*: Text

files :: Table File
files = table "files" $ autoPrimary "file_id"
                          :*: required "file_path"

(file_id :*: file_path) = selectors files


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

type AmbiguousName = RowID :*: Text :*: Int :*: Int :*: Int :*: Int :*: RowID

ambiguousNames :: Table AmbiguousName
ambiguousNames = table "ambiguous_names" $ required "amb_scope"
                                             :*: required "amb_file"
                                             :*: required "amb_start_row"
                                             :*: required "amb_start_col"
                                             :*: required "amb_end_row"
                                             :*: required "amb_end_col"
                                             :*: required "amb_module" `fk` (modules, module_id)
( amb_scope
    :*: amb_file
    :*: amb_start_row
    :*: amb_start_col
    :*: amb_end_row
    :*: amb_end_col
    :*: amb_module ) = selectors ambiguousNames

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
