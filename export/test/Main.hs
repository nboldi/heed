module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit

import Language.Haskell.Heed.Export.Export

main = defaultMain languageTestsCases

languageTestsCases :: TestTree
languageTestsCases
  = testGroup "all tests"
      [ testGroup "language tests" (map (\mn -> testCase mn $ checkExport mn) languageTests)
      , testGroup "rename definition tests" (map (\(mn,_,_) -> testCase mn $ checkExport mn) renameDefinitionTests)
      ]

checkExport :: String -> IO ()
checkExport modName = exportSrcFile "examples" modName True

languageTests =
  [ "Decl.AmbiguousFields"
  , "Decl.AnnPragma"
  , "Decl.ClassInfix"
  , "Decl.ClosedTypeFamily"
  , "Decl.CompletePragma"
  , "Decl.CtorOp"
  , "Decl.DataFamily"
  , "Decl.DataType"
  , "Decl.DataInstanceGADT"
  , "Decl.DataTypeDerivings"
  , "Decl.DefaultDecl"
  , "Decl.FunBind"
  , "Decl.FunctionalDeps"
  , "Decl.FunGuards"
  , "Decl.GADT"
  , "Decl.GadtConWithCtx"
  , "Decl.InfixAssertion"
  , "Decl.InfixInstances"
  , "Decl.InfixPatSyn"
  , "Decl.InjectiveTypeFamily"
  , "Decl.InlinePragma"
  , "Decl.InstanceFamily"
  , "Decl.InstanceOverlaps"
  , "Decl.InstanceSpec"
  , "Decl.LocalBindings"
  , "Decl.LocalBindingInDo"
  , "Decl.LocalFixity"
  , "Decl.MinimalPragma"
  , "Decl.MultipleFixity"
  , "Decl.MultipleSigs"
  , "Decl.OperatorBind"
  , "Decl.OperatorDecl"
  , "Decl.ParamDataType"
  , "Decl.PatternBind"
  , "Decl.PatternSynonym"
  , "Decl.RecordPatternSynonyms"
  , "Decl.RecordType"
  , "Decl.RewriteRule"
  , "Decl.SpecializePragma"
  , "Decl.StandaloneDeriving"
  , "Decl.TypeClass"
  , "Decl.TypeClassMinimal"
  , "Decl.TypeFamily"
  , "Decl.TypeFamilyKindSig"
  , "Decl.TypeInstance"
  , "Decl.TypeRole"
  , "Decl.TypeSynonym"
  , "Decl.ValBind"
  , "Decl.ViewPatternSynonym"
  , "Expr.ArrowNotation"
  , "Expr.Case"
  , "Expr.DoNotation"
  , "Expr.GeneralizedListComp"
  , "Expr.EmptyCase"
  , "Expr.FunSection"
  , "Expr.If"
  , "Expr.LambdaCase"
  , "Expr.ListComp"
  , "Expr.MultiwayIf"
  , "Expr.Negate"
  , "Expr.Operator"
  , "Expr.ParenName"
  , "Expr.ParListComp"
  , "Expr.PatternAndDo"
  , "Expr.RecordPuns"
  , "Expr.RecordWildcards"
  , "Expr.RecursiveDo"
  , "Expr.Sections"
  , "Expr.SemicolonDo"
  , "Expr.StaticPtr"
  , "Expr.TupleSections"
  , "Expr.UnboxedSum"
  , "Module.Simple"
  , "Module.GhcOptionsPragma"
  , "Module.Export"
  , "Module.ExportSubs"
  , "Module.ExportModifiers"
  , "Module.NamespaceExport"
  , "Module.Import"
  , "Module.ImportOp"
  , "Module.LangPragmas"
  , "Module.PatternImport"
  , "Pattern.Backtick"
  , "Pattern.Constructor"
  , "Pattern.Infix"
  , "Pattern.NestedWildcard"
  , "Pattern.NPlusK"
  , "Pattern.OperatorPattern"
  , "Pattern.Record"
  , "Pattern.UnboxedSum"
  , "Type.Bang"
  , "Type.Builtin"
  , "Type.Ctx"
  , "Type.ExplicitTypeApplication"
  , "Type.Forall"
  , "Type.Primitives"
  , "Type.TupleAssert"
  , "Type.TypeOperators"
  , "Type.Unpack"
  , "Type.Wildcard"
  , "TH.Brackets"
  , "TH.QuasiQuote.Define"
  , "TH.QuasiQuote.Use"
  , "TH.Splice.Define"
  , "TH.Splice.Use"
  , "TH.CrossDef"
  , "TH.ClassUse"
  , "TH.Splice.UseImported"
  , "TH.Splice.UseQual"
  , "TH.Splice.UseQualMulti"
  , "TH.LocalDefinition"
  , "TH.MultiImport"
  , "TH.NestedSplices"
  , "TH.Quoted"
  , "TH.WithWildcards"
  , "TH.DoubleSplice"
  , "TH.GADTFields"
  ]

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
  , ("Refactor.RenameDefinition.RoleAnnotation", "4:11-4:12", "AA")
  , ("Refactor.RenameDefinition.TypeBracket", "6:6-6:7", "B")
  , ("Refactor.RenameDefinition.ValBracket", "8:11-8:12", "B")
  , ("Refactor.RenameDefinition.FunnyDo", "3:1-3:2", "aaa")
  , ("Refactor.RenameDefinition.RenameModuleAlias", "3:21-3:23", "L")
  , ("Refactor.RenameDefinition.MergeFields", "3:14-3:15", "y")
  , ("Refactor.RenameDefinition.MergeFields_RenameY", "3:34-3:35", "x")
  , ("Refactor.RenameDefinition.PatternSynonym", "6:9", "ArrowAppl")
  , ("Refactor.RenameDefinition.PatternSynonymTypeSig", "6:9", "ArrowAppl")
  , ("Refactor.RenameDefinition.QualImport", "5:1", "intercalate")
  ]
