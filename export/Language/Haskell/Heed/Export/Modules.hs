{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Heed.Export.Modules where

import Language.Haskell.Heed.Export.Declarations
import Language.Haskell.Heed.Export.Bindings
import Language.Haskell.Heed.Export.Utilities
import Language.Haskell.Heed.Schema as Schema

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Data
import Bag
import GHC
import Module as GHC
import HsSyn
import FieldLabel
import BasicTypes
import FastString
import SrcLoc
import HsImpExp as GHC

exportTcModule :: Exporter TypecheckedSource
exportTcModule binds
  = mapM_ exportBinding (bagToList binds)

exportRnModule :: HsName n => Exporter (HsGroup n, [LImportDecl n], Maybe [LIE n], Maybe LHsDocString)
exportRnModule (gr,imports,exps,_)
  = do rn <- asks moduleRange
       addToScope rn $ do
         mapM_ writeModImport imports
         export Schema.Module noSrcSpan [ return ()
                                        , maybe (return ()) (mapM_ exportExportSpec) exps
                                        , mapM_ exportImportDecl imports
                                        , exportDeclarationGroup gr
                                        ]

exportModule :: HsName n => Exporter (Located (HsModule n))
exportModule (L l (HsModule name exports imports decls deprec _))
  -- TODO: process deprec
  = export Schema.Module l [ maybe (return ()) exportModuleName name
                           , maybe (return ()) (mapM_ (mapM_ exportExportSpec)) exports
                           , mapM_ exportImportDecl imports
                           , mapM_ exportDeclaration decls
                           ]

exportExportSpec :: HsName n => Exporter (LIE n)
exportExportSpec (L l (IEModuleContents mn)) = export ModuleExport l [ exportModuleName mn ]
exportExportSpec (L l ie) = export NormalExport l [ exportIESpec (L l ie) ]

exportImportDecl :: HsName n => Exporter (LImportDecl n)
exportImportDecl (L l ie) | ideclImplicit ie = return ()
exportImportDecl imp@(L l (GHC.ImportDecl _ name pkg isSrc isSafe isQual _ declAs declHiding))
  = export Schema.ImportDecl l [ when isSrc (export ImportSource l [])
                               , when isQual (export ImportQualified l [])
                               , when isSafe (export ImportSafe l [])
                               , maybe (return ()) (writeStringAttribute . unpackFS . sl_fs) pkg
                               , exportModuleName name
                               , maybe (return ()) exportModuleName declAs
                               , maybe (return ()) exportImportSpec declHiding
                               ]

exportImportSpec :: HsName n => Exporter (Bool, Located [LIE n])
exportImportSpec (True, (L l ies)) = export ImportHiding l [ mapM_ exportIESpec ies ]
exportImportSpec (False, (L l ies)) = export ImportSpecList l [ mapM_ exportIESpec ies ]

exportIESpec :: HsName n => Exporter (LIE n)
exportIESpec (L l (IEVar n)) = export IESpec l [ exportName (getWrappedName n) ]
exportIESpec (L l (IEThingAbs n)) = export IESpec l [ exportName (getWrappedName n) ]
exportIESpec (L l (IEThingAll n)) = export IESpec l [ exportName (getWrappedName n), export IESubspecAll l [] ]
exportIESpec (L l (IEThingWith n _ ls flds))
  = export IESpec l [ exportName (getWrappedName n)
                    , export IESubspecList l [ mapM_ (exportName . getWrappedName) ls
                                             , mapM_ (exportName . fmap flSelector) flds ] ]

getWrappedName :: Located (IEWrappedName n) -> Located n
getWrappedName (L _ (IEName n)) = n
getWrappedName (L _ (IEPattern n)) = n
getWrappedName (L _ (IEType n)) = n

exportModuleName :: Exporter (Located GHC.ModuleName)
exportModuleName (L l mn) = export Schema.ModuleName l [ writeStringAttribute (moduleNameString mn) ]
