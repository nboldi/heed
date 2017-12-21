{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Core where

import Control.Monad
import Control.Exception (throwIO)
import Control.Monad.Trans.Class
import Data.Data
import Data.List

import Data.Text

import Database.Selda
import Control.Monad.Catch
import GHC
import GhcMonad

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

-- * Syntax

nodes :: Table (Text :*: Text :*: Text :*: Int :*: Int :*: Int :*: Int :*: Maybe Text)
nodes = table "nodes" $ required "node_type" 
                          :*: required "ctor" 
                          :*: required "file"
                          :*: required "start_row"
                          :*: required "start_col"
                          :*: required "end_row"
                          :*: required "end_col"
                          :*: optional "parent_handle"
( node_type 
    :*: node_ctor 
    :*: node_file 
    :*: node_start_row 
    :*: node_start_col 
    :*: node_end_row 
    :*: node_end_col 
    :*: node_parent_handle ) = selectors nodes

-- * Semantic information

names :: Table ( Text :*: Int :*: Int :*: Int :*: Int :*: Maybe Text :*: Maybe Int :*: Maybe Int :*: Maybe Int :*: Maybe Int :*: Text :*: Text :*: Text )
names = table "name_infos" $ required "file"
                               :*: required "start_row"
                               :*: required "start_col"
                               :*: required "end_row"
                               :*: required "end_col"
                               :*: optional "def_file"
                               :*: optional "def_start_row"
                               :*: optional "def_start_col"
                               :*: optional "def_end_row"
                               :*: optional "def_end_col"
                               :*: required "namespace" 
                               :*: required "name"
                               :*: required "uniq"
( name_file 
    :*: name_start_row 
    :*: name_start_col 
    :*: name_end_row 
    :*: name_end_col
    :*: def_file
    :*: def_start_row
    :*: def_start_col
    :*: def_end_row
    :*: def_end_col
    :*: name_namespace
    :*: name_str
    :*: name_uniq ) = selectors names

scopes :: Table ( RowID :*: Text :*: Int :*: Int :*: Int :*: Int )
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


scopeNames :: Table ( RowID :*: Text :*: Text :*: Text )
scopeNames = table "scope_names" $ required "scope_id"
                                    :*: required "name"
                                    :*: required "namespace" 
                                    :*: required "uniq"
( scope_name_id 
    :*: scope_name
    :*: scope_namespace
    :*: scope_uniq ) = selectors scopeNames

-- * Lexical information

tokens :: Table (Text :*: Int :*: Int :*: Int :*: Int :*: Text)
tokens = table "tokens" $ required "file"
                            :*: required "start_row"
                            :*: required "start_col"
                            :*: required "end_row"
                            :*: required "end_col"
                            :*: required "token"
                          

comments :: Table (Text :*: Int :*: Int :*: Int :*: Int :*: Text :*: Text)
comments = table "comments" $ required "file"
                                :*: required "start_row"
                                :*: required "start_col"
                                :*: required "end_row"
                                :*: required "end_col"
                                :*: required "type"
                                :*: required "content"

