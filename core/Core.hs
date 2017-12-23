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

nodes :: Table (RowID :*: Maybe RowID :*: Int :*: Int :*: Text :*: Int :*: Int :*: Int :*: Int :*: Maybe Int)
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

-- * Semantic information

names :: Table ( Maybe RowID :*: RowID :*: Maybe Text :*: Maybe Int :*: Maybe Int :*: Maybe Int :*: Maybe Int
                   :*: Text :*: Text :*: Text :*: Bool )
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

