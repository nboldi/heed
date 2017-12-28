{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Heed.Export.Statements where

import Language.Haskell.Heed.Export.Patterns
import Language.Haskell.Heed.Export.Names
import {-# SOURCE #-} Language.Haskell.Heed.Export.Bindings
import {-# SOURCE #-} Language.Haskell.Heed.Export.Expressions
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Export.Schema

import Data.Data
import HsExpr
import HsPat
import HsBinds
import SrcLoc


exportDoStatement :: (HsName n, Data e) => Exporter e -> Exporter (Located (Stmt n e))
exportDoStatement exporter (L l (BindStmt pat expr _ _ _))
  = export BindS l [ exportPattern pat, exporter expr ]
exportDoStatement exporter (L l (BodyStmt expr _ _ _)) = export BodyS l [ exporter expr ]
exportDoStatement exporter (L l (LastStmt expr _ _)) = export BodyS l [ exporter expr ]
exportDoStatement exporter (L l (LetStmt binds)) = export LetS l [ exportLocalBinds binds ]
exportDoStatement exporter (L l (RecStmt { recS_stmts = stmts }))
  = export RecursiveS l [ mapM_ (exportDoStatement exporter) stmts ]
exportDoStatement _ (L l stmt) = exportError "do statement" stmt

exportListCompStatements :: HsName n => Exporter [Located (Stmt n (LHsExpr n))]
exportListCompStatements [unLoc -> ParStmt blocks _ _ _, unLoc -> (LastStmt {})]
  = mapM_ (\(ParStmtBlock stmts _ _) -> export ListCompBody (combineLocated stmts)
                                          [ scopedSequence exportListCompStatement stmts ] ) blocks
exportListCompStatements stmts = export ListCompBody (combineLocated stmts)
                                   [ scopedSequence exportListCompStatement stmts ]


exportListCompStatement :: HsName n => Exporter (Located (Stmt n (LHsExpr n)))
exportListCompStatement (L l (TransStmt { trS_form = ThenForm, trS_using = using, trS_by = by }))
  = export ThenS l [ exportExpression using, maybe (return ()) exportExpression by ]
exportListCompStatement (L l (TransStmt { trS_form = GroupForm, trS_using = using, trS_by = by }))
  = export GroupS l [ exportExpression using, maybe (return ()) exportExpression by ]
exportListCompStatement (L l stmt)
  = export NormalS l [ exportDoStatement exportExpression (L l stmt) ]

