{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Heed.Export.Schema where

import Data.Data
import qualified Language.Haskell.TH.Syntax as TH

class Data t => Schema t where
  typeId :: t -> Int

data Module = Module deriving Data

data Declaration = BindingD deriving Data

data Binding = FunctionB deriving Data

data Match = Match deriving Data

data LocalBindings = LocalBindings deriving Data

data LocalBinding = LocalValueBind | LocalTypeSignature | LocalFixitySignature deriving Data

data Alternative = Alternative deriving Data

data Rhs = Unguarded deriving Data

data CaseRhs = UnguardedC deriving Data

data Expression = VarE | InfixAppE | LiteralE | LambdaE | LambdaCaseE | AppE | PrefixAppE
                | RightSectionE | LeftSectionE | ParenE | TupleE | TupleSectionE | UnboxedTupleE
                | UnboxedTupleSectionE | IfE | MultiIfE | LetE | ListE | ParallelArrayE
                | RecordConstructE | RecordUpdateE | TypedE | EnumE | ParallelArrayEnumE
                | BracketE | QuasiQouteE | SpliceE | StaticE | TypeApplicationE | DoE | MDoE
                | ListCompE | ParallelArrayCompE
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


data Name = Normal | Paren
  deriving Data

data Operator = NormalOp | Backtick
  deriving Data

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

-- generate indexed instances for Schema
$( concat <$> mapM (\(i,t) -> [d| instance Schema $(return $ TH.ConT t) where
                                   typeId _ = $(return $ TH.LitE $ TH.IntegerL i) |])
     (zip [0..] [ ''Module, ''Declaration, ''Binding, ''Match, ''Alternative, ''Rhs, ''CaseRhs
                , ''Expression, ''Pattern, ''PatternField, ''KindSignature, ''Kind, ''Literal
                , ''Name, ''Operator, ''Type, ''TypeSignature, ''FixitySignature, ''LocalBindings
                , ''LocalBinding, ''FieldUpdates, ''FieldUpdate, ''FieldWildcard, ''Promoted
                , ''TypeVariable, ''Context, ''Predicate, ''Statement, ''ListComprehensionBody
                , ''ListComprehensionStatement ])
 )
