{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Heed.Schema where

import Data.Data
import qualified Language.Haskell.TH.Syntax as TH

class Data t => Schema t where
  typeId :: t -> Int

data Module = Module deriving Data

data Declaration = BindingD | TypeSynonymD | DataD | TypeSignatureD | PatternSignatureD
                 | FixitySignatureD | PragmaD | PatternSynonymD | GDataD | ClassD | InstanceD
                 | ClosedFamilyD | TypeFamilyD | DataInstanceD | TypeInstanceD | DerivingD
                 | RoleD | DefaultD | ForeignImportD | ForeignExportD | SpliceD
  deriving Data

data Binding = FunctionB | SimpleB deriving Data

data Match = Match deriving Data

data LocalBindings = LocalBindings deriving Data

data LocalBinding = LocalValueBind | LocalTypeSignature | LocalFixitySignature | LocalPragma
  deriving Data

data Alternative = Alternative deriving Data

data Rhs = UnguardedRhs | GuardedRhss deriving Data

data Expression = VarE | InfixAppE | LiteralE | LambdaE | LambdaCaseE | AppE | PrefixAppE
                | RightSectionE | LeftSectionE | ParenE | TupleE | TupleSectionE | UnboxedTupleE
                | UnboxedTupleSectionE | IfE | MultiIfE | LetE | ListE | ParallelArrayE
                | RecordConstructE | RecordUpdateE | TypedE | EnumE | ParallelArrayEnumE
                | BracketE | QuasiQouteE | SpliceE | StaticE | TypeApplicationE | DoE | MDoE
                | ListCompE | ParallelArrayCompE | UnboxedSumE | WildPatE | ProcE
  deriving Data


data Pattern = VariableP | WildcardP | LazyP | AsP | ParenP | BangP | ListP | TupleP | UnboxedTupleP
             | ParallelArrayP | PrefixConstructorP | RecordConstructorP | ViewP | QuasiQuotationP
             | SpliceP | TypedP | SumP | LiteralP
  deriving Data

data PatternField = Prefix | Pun deriving Data

data KindSignature = KindSignature deriving Data

data Kind = StarK | UnboxK | ParenK | FunctionK | ApplicationK | InfixApplicationK | VarK
          | ListK | TupleK | PromotedK | TypeK
  deriving Data

data Promoted = PromotedInt | PromotedString | PromotedList | PromotedTuple
  deriving Data

data TypeSignature = TypeSignature deriving Data

data FixitySignature = FixitySignatureLeft | FixitySignatureRight | FixitySignature deriving Data

data Type = ForallT | ContextT | VariableT | ApplicationT | FunctionT | ListT | ParallelArrayT
          | TupleT | UnboxedTupleT | ParenT | InfixT | KindedT | QuasiQouteT | SpliceT | UnpackT
          | NoUnpackT | BangT | LazyT | PromotedT | WildcardT | SumT
  deriving Data

data Literal = Character | PrimitiveCharacter | String | PrimitiveString | PrimitiveInt
             | Word | PrimitiveInt64 | PrimitiveWord64 | PrimitiveFloat | PrimitiveDouble
             | Integral | Fractional | OverloadedString
  deriving Data


data Name = NormalName | ParenName
  deriving Data

data Operator = NormalOperator | BacktickOperator
  deriving Data

data FieldPatterns = FieldPatterns deriving Data

data FieldUpdates = FieldUpdates deriving Data

data FieldWildcard = FieldWildcard deriving Data

data FieldUpdate = FieldPun | NormalFieldUpdate
  deriving Data

data TypeVariable = NormalTV | KindedTV
  deriving Data

data Context = Context
  deriving Data

data Predicate = ClassPredicate | TuplePredicate | TypeEqPredicate | ImplicitPredicate
               | InfixPredicate | WildcardPredicate
  deriving Data

data Statement = BindS | BodyS | LetS | RecursiveS
  deriving Data

data ListComprehensionBody = ListCompBody
  deriving Data

data ListComprehensionStatement = ThenS | GroupS | NormalS
  deriving Data

data ImplicitName = ImplicitName
  deriving Data

data Qualifiers = Qualifiers
  deriving Data

data UnqualifiedName = UnqualifiedName
  deriving Data

data Splice = Splice
  deriving Data

data QuasiQuotation = QuasiQuotation
  deriving Data

data Bracket = ExprBracket | PatternBracket | TypeBracket | DeclarationBracket
  deriving Data

data GuardStmt = GuardBind | GuardBody | GuardLet
  deriving Data

data InlinePragma = InlinablePragma | NoInlinePragma | InlinePragma
  deriving Data

data Phase = NoPhase | AfterPhase | BeforePhase
  deriving Data

data Constructor = Constructor | RecordConstructor | InfixConstructor
  deriving Data

data FieldDecl = FieldDecl deriving Data

data Derivings = Derivings deriving Data

data DerivingStrategy = StockDerivingStrat | AnyClassDerivingStrat | NewtypeDerivingStrat
  deriving Data

data InstanceHead = ConstructorIH | ApplicationIH | ParenIH | InfixIH
  deriving Data

data SpecializePragma = SpecializePragma deriving Data

data CompletePragma = CompletePragma deriving Data

data PatternSynonymLhs = PrefixPatSynLhs | InfixPatSynLhs | RecordPatSynLhs
  deriving Data

data PatternSynonymRhs = BidirectionalPatSyn | ExplicitBidirectionalPatSyn | UnidirectionalPatSyn
  deriving Data

data NewtypeKeyword = NewType | DataType deriving Data

data GADTType = GADTRecordType | GADTNormalType deriving Data

data GADTConstructor = GADTConstructor deriving Data

data DeclarationHead = PrefixDH | InfixDH deriving Data

data FunctionalDependency = FunctionalDependency deriving Data

data ClassElement = ClassBinding | ClassTypeFamily | ClassTypeSignature | ClassFixitySignature
                  | ClassDefaultSignature | ClassMinimalSignature | ClassInlinePragma
                  | ClassTypeFamilyDefault
  deriving Data

data MinimalFormula = MinimalName | MinimalAnd | MinimalOr | MinimalParen
  deriving Data

data InstanceRule = InstanceRule deriving Data

data TypeEquation = InfixTypeEquation | PrefixTypeEquation deriving Data

data TypeFamilySignature = TFKindSig | TFTyVarSig | TFInjectivitySig deriving Data

data TypeFamily = TypeFamily | DataFamily deriving Data

data CallConv = CCC | CApiCC | StdCC | JavaScriptCC | PrimCC deriving Data

data Safety = Safe | Interruptible | Unsafe deriving Data

data Overlap = DisableOverlap | Overlappable | Overlapping | Overlaps | IncoherentOverlap
  deriving Data

data Role = NominalRole | RepresentationalRole | PhantomRole deriving Data

data InstanceElement = InstanceBinding | InstanceTypeSig | InstanceSpecialize
                     | InstanceSpecializeInstance | InstanceInline | InstanceTypeFamily
                     | InstanceDataDecl | InstanceGDataDecl
  deriving Data

data InjectivityAnnot = InjectivityAnnot deriving Data

data TopLevelPragma = RulePragma deriving Data

data RewriteRule = RewriteRule deriving Data

data RuleVar = RuleVar | RuleSigVar deriving Data

data ModuleName = ModuleName deriving Data

data ImportDecl = ImportDecl deriving Data

data ImportSource = ImportSource deriving Data
data ImportSafe = ImportSafe deriving Data
data ImportQualified = ImportQualified deriving Data

data ExportSpec = NormalExport | ModuleExport deriving Data

data ImportSpec = ImportHiding | ImportSpecList deriving Data

data IESpec = IESpec deriving Data

data IESubSpec = IESubspecAll | IESubspecList deriving Data

data Command = ArrowAppCmd | ArrowFormCmd | AppCmd | LambdaCmd | CaseCmd | ParenCmd | IfCmd | LetCmd
             | DoCmd
  deriving Data

data Arrow = RightApp | LeftApp | RightHighApp | LeftHighApp deriving Data

-- generate indexed instances for Schema
$( concat <$> mapM (\(i,t) -> [d| instance Schema $(return $ TH.ConT t) where
                                   typeId _ = $(return $ TH.LitE $ TH.IntegerL i) |])
     (zip [0..] [ ''Module, ''Declaration, ''Binding, ''Match, ''Alternative, ''Rhs
                , ''Expression, ''Pattern, ''PatternField, ''KindSignature, ''Kind, ''Literal
                , ''Name, ''Operator, ''Type, ''TypeSignature, ''FixitySignature, ''LocalBindings
                , ''LocalBinding, ''FieldUpdates, ''FieldUpdate, ''FieldWildcard, ''Promoted
                , ''TypeVariable, ''Context, ''Predicate, ''Statement, ''ListComprehensionBody
                , ''ListComprehensionStatement, ''ImplicitName, ''Qualifiers, ''UnqualifiedName
                , ''Splice, ''QuasiQuotation, ''Bracket, ''GuardStmt, ''InlinePragma, ''Phase
                , ''Constructor, ''FieldDecl, ''Derivings, ''DerivingStrategy, ''InstanceHead
                , ''SpecializePragma, ''CompletePragma, ''PatternSynonymLhs, ''PatternSynonymRhs
                , ''NewtypeKeyword, ''GADTType, ''GADTConstructor, ''DeclarationHead
                , ''FunctionalDependency, ''ClassElement, ''MinimalFormula, ''InstanceRule
                , ''TypeEquation, ''TypeFamilySignature, ''TypeFamily, ''CallConv, ''Safety
                , ''Overlap, ''Role, ''InstanceElement, ''InjectivityAnnot, ''TopLevelPragma
                , ''RewriteRule, ''RuleVar, ''ModuleName, ''ImportDecl, ''ImportSafe, ''ImportSource
                , ''ImportQualified, ''ExportSpec, ''ImportSpec, ''IESpec, ''IESubSpec, ''FieldPatterns
                , ''Command, ''Arrow
                ])
 )
