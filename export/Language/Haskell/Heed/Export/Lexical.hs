module Language.Haskell.Heed.Export.Lexical where

import Data.Text (pack)
import GHC
import Database.Selda

import Core
import Language.Haskell.Heed.Export.Utilities

insertTokens :: [((SrcSpan, AnnKeywordId), [SrcSpan])] -> SeldaT Ghc ()
insertTokens ghcTokens = do
  let toInsert = 
        map (\((_, keyw), [sp]) -> 
                 let (file, start_row, start_col, end_row, end_col) = spanData sp
                  in pack file :*: start_row :*: start_col :*: end_row :*: end_col :*: pack (show keyw)
            ) ghcTokens
  insert_ tokens toInsert

insertComments :: [Located AnnotationComment] -> SeldaT Ghc ()
insertComments ghcComments = do
  let toInsert = 
        map (\(L l comm) -> let (file, start_row, start_col, end_row, end_col) = spanData l
                                (kind, text) = categorizeComment comm
                             in pack file :*: start_row :*: start_col :*: end_row :*: end_col :*: pack text :*: pack kind
            ) ghcComments
  insert_ comments toInsert
        
categorizeComment :: AnnotationComment -> (String, String)
categorizeComment (AnnDocCommentNext str) = ("AnnDocCommentNext", str)
categorizeComment (AnnDocCommentPrev str) = ("AnnDocCommentPrev", str)
categorizeComment (AnnDocCommentNamed str) = ("AnnDocCommentNamed", str)
categorizeComment (AnnDocSection _ str) = ("AnnDocSection", str)
categorizeComment (AnnDocOptions str) = ("AnnDocOptions", str)
categorizeComment (AnnLineComment str) = ("AnnLineComment", str)
categorizeComment (AnnBlockComment str) = ("AnnBlockComment", str)
