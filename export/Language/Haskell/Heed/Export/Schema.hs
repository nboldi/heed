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
                | BracketE | QuasiQouteE | SpliceE | StaticE | TypeApplicationE
  deriving Data


data Pattern = VariableP | WildcardP | LazyP | AsP | ParenP | BangP | ListP | TupleP | UnboxedTupleP
             | ParallelArrayP | PrefixConstructorP | RecordConstructorP | ViewP | QuasiQuotationP
             | SpliceP | TypedP | SumP | LiteralP
  deriving Data

data PatternField = Prefix | Pun deriving Data

data KindSignature = KindSignature deriving Data

data Kind = ParenK | FunctionK | ApplicationK | InfixApplicationK deriving Data

data TypeSignature = TypeSignature deriving Data

data FixitySignature = FixitySignatureLeft | FixitySignatureRight | FixitySignature deriving Data

data Type = ForallT deriving Data

data Literal = Character
             | PrimitiveCharacter
             | String
             | PrimitiveString
             | PrimitiveInt
             | Word
             | PrimitiveInt64
             | PrimitiveWord64
             | PrimitiveFloat
             | PrimitiveDouble
             | Integral
             | Fractional
             | OverloadedString
  deriving Data


data Name = Normal | Paren
  deriving Data

data Operator = NormalOp | Backtick
  deriving Data

data FieldUpdates = FieldUpdates deriving Data

data FieldWildcard = FieldWildcard deriving Data

data FieldUpdate = FieldPun | NormalFieldUpdate
  deriving Data

$( concat <$> mapM (\(i,t) -> [d| instance Schema $(return $ TH.ConT t) where
                                   typeId _ = $(return $ TH.LitE $ TH.IntegerL i) |])
     (zip [0..] [ ''Module, ''Declaration, ''Binding, ''Match, ''Alternative, ''Rhs, ''CaseRhs
                , ''Expression, ''Pattern, ''PatternField, ''KindSignature, ''Kind, ''Literal
                , ''Name, ''Operator, ''Type, ''TypeSignature, ''FixitySignature, ''LocalBindings
                , ''LocalBinding, ''FieldUpdates, ''FieldUpdate, ''FieldWildcard ])
 )
