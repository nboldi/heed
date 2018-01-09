{-# LANGUAGE LambdaCase #-}
module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit
import System.FilePath
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import Control.Monad.IO.Class
import Database.Selda
import Database.Selda.SQLite

import Language.Haskell.Heed.Refactor.RenameDefinition
import Language.Haskell.Heed.Export.Export

main = defaultMain refactoringTestCases

refactoringTestCases :: TestTree
refactoringTestCases
  = testGroup "all tests"
      [ testGroup "rename definition tests"
          ( map (\td@(mn,_,_) -> testCase mn $ checkCorrectlyRefactored td) renameDefinitionTests
              ++ map (\td@(mn,_,_) -> testCase mn $ checkRefactoringFails td) wrongRenameDefinitionTests )
      ]

root = "examples"

checkCorrectlyRefactored :: (String, String, String) -> IO ()
checkCorrectlyRefactored (mn, span, newName)
  = do originalSource <- liftIO $ withBinaryFile fileName ReadMode (\h -> hSetEncoding h utf8 >> hGetContents h)
       expectedSource <- liftIO $ withBinaryFile resFileName ReadMode (\h -> hSetEncoding h utf8 >> hGetContents h)
       exportSrcFile root mn True
       rewr <- withSQLite "haskell.db" $ renameDefinition newName span
       case rewr of
         Right rewritings -> let modified = applyRewritings rewritings originalSource
                              in assertEqual "Expected and actual results doesn't match" expectedSource modified
         Left err -> assertFailure err
  where fileName = root </> map (\case '.' -> pathSeparator; c -> c) mn ++ ".hs"
        resFileName = root </> map (\case '.' -> pathSeparator; c -> c) mn ++ "_res.hs"

checkRefactoringFails :: (String, String, String) -> IO ()
checkRefactoringFails (mn, span, newName) = do
  exportSrcFile root mn True
  rewr <- withSQLite "haskell.db" $ renameDefinition newName span
  case rewr of
    Right _ -> assertFailure "The refactoring should fail"
    Left _ -> return ()

renameDefinitionTests =
  [ ("Refactor.RenameDefinition.AmbiguousFields", "4:14-4:15", "xx")
  , ("Refactor.RenameDefinition.RecordField", "3:22-3:23", "xCoord")
  , ("Refactor.RenameDefinition.Constructor", "3:14-3:19", "Point2D")
  , ("Refactor.RenameDefinition.Type", "5:16-5:16", "Point2D")
  , ("Refactor.RenameDefinition.Function", "3:1-3:2", "q")
  , ("Refactor.RenameDefinition.AccentName", "3:1-3:2", "á")
  , ("Refactor.RenameDefinition.QualName", "3:1-3:2", "q")
  , ("Refactor.RenameDefinition.BacktickName", "3:1-3:2", "g")
  , ("Refactor.RenameDefinition.ParenName", "4:3-4:5", "<->")
  , ("Refactor.RenameDefinition.RecordWildcards", "4:32-4:33", "yy")
  , ("Refactor.RenameDefinition.RecordPatternSynonyms", "4:16-4:17", "xx")
  , ("Refactor.RenameDefinition.ClassMember", "7:3-7:4", "q")
  , ("Refactor.RenameDefinition.LocalFunction", "4:5-4:6", "g")
  , ("Refactor.RenameDefinition.LayoutAware", "3:1-3:2", "main")
  , ("Refactor.RenameDefinition.FormattingAware", "3:1-3:2", "aa")
  , ("Refactor.RenameDefinition.Arg", "4:3-4:4", "y")
  , ("Refactor.RenameDefinition.FunTypeVar", "3:6-3:7", "x")
  , ("Refactor.RenameDefinition.FunTypeVarLocal", "5:10-5:11", "b")
  , ("Refactor.RenameDefinition.ClassTypeVar", "3:9-3:10", "f")
  , ("Refactor.RenameDefinition.TypeOperators", "4:13-4:15", "x1")
  , ("Refactor.RenameDefinition.NoPrelude", "4:1-4:2", "map")
  , ("Refactor.RenameDefinition.UnusedDef", "3:1-3:2", "map")
  , ("Refactor.RenameDefinition.SameCtorAndType", "3:6-3:13", "P2D")
  -- , ("Refactor.RenameDefinition.RoleAnnotation", "4:11-4:12", "AA")
  -- , ("Refactor.RenameDefinition.TypeBracket", "6:6-6:7", "B")
  -- , ("Refactor.RenameDefinition.ValBracket", "8:11-8:12", "B")
  , ("Refactor.RenameDefinition.FunnyDo", "3:1-3:2", "aaa")
  -- , ("Refactor.RenameDefinition.RenameModuleAlias", "3:21-3:23", "L")
  , ("Refactor.RenameDefinition.MergeFields", "3:14-3:15", "y")
  , ("Refactor.RenameDefinition.MergeFields_RenameY", "3:34-3:35", "x")
  , ("Refactor.RenameDefinition.PatternSynonym", "6:9", "ArrowAppl")
  , ("Refactor.RenameDefinition.PatternSynonymTypeSig", "6:9", "ArrowAppl")
  -- , ("Refactor.RenameDefinition.QualImport", "5:1", "intercalate")
  ]

wrongRenameDefinitionTests =
  [ ("Refactor.RenameDefinition.LibraryFunction", "4:5-4:7", "identity")
  , ("Refactor.RenameDefinition.NameClash", "5:9-5:10", "h")
  , ("Refactor.RenameDefinition.NameClash", "3:1-3:2", "map")
  , ("Refactor.RenameDefinition.WrongName", "4:1-4:2", "F")
  , ("Refactor.RenameDefinition.WrongName", "4:1-4:2", "++")
  , ("Refactor.RenameDefinition.WrongName", "7:6-7:7", "x")
  , ("Refactor.RenameDefinition.WrongName", "7:6-7:7", ":+:")
  , ("Refactor.RenameDefinition.WrongName", "7:10-7:11", "x")
  , ("Refactor.RenameDefinition.WrongName", "9:6-9:7", "A")
  , ("Refactor.RenameDefinition.WrongName", "9:19-9:19", ".+++.")
  , ("Refactor.RenameDefinition.WrongName", "11:3-11:3", ":+++:")
  -- , ("Refactor.RenameDefinition.IllegalQualRename", "4:30-4:34", "Bl")
  , ("Refactor.RenameDefinition.CrossRename", "4:1-4:2", "g")
  , ("Refactor.RenameDefinition.MergeFields", "5:16-5:18", "y2") -- fld in the same ctor
  , ("Refactor.RenameDefinition.MergeFields", "5:30-5:32", "x2") -- fld in the same ctor
  , ("Refactor.RenameDefinition.MergeFields", "5:16-5:18", "y") -- fld belongs to other type
  , ("Refactor.RenameDefinition.MergeFields", "7:16-7:18", "y3") -- types does not match
  , ("Refactor.RenameDefinition.MergeFields", "7:38-7:40", "x3") -- types does not match
  -- , ("Refactor.RenameDefinition.QualImportAlso", "6:1", "intercalate") -- there is a non-qualified import
  ]