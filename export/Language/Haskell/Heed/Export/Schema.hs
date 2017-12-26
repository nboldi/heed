{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Heed.Export.Schema where

import Data.Data
import Language.Haskell.TH.Syntax (Type(..), Exp(..), Lit(..))

class Data t => Schema t where
  typeId :: t -> Int

data Module = Module deriving Data

data Declaration = BindingD deriving Data

data Binding = FunctionB deriving Data

data Match = Match deriving Data

data Alternative = Alternative deriving Data

data Rhs = Unguarded deriving Data

data CaseRhs = UnguardedC deriving Data

data Expression = VarE | InfixAppE | LiteralE | LambdaE | LambdaCaseE | AppE | PrefixAppE
                | RightSectionE | LeftSectionE | ParenE | TupleE | TupleSectionE | UnboxedTupleE
                | UnboxedTupleSectionE | IfE | MultiIfE | LetE
  deriving Data


data Pattern = VariableP | WildcardP | LazyP | AsP | ParenP | BangP | ListP | TupleP | UnboxedTupleP
             | ParallelArrayP | PrefixConstructorP | RecordConstructorP | ViewP | QuasiQuotationP
             | SpliceP | TypedP | SumP | LiteralP
  deriving Data

data PatternField = Prefix | Pun deriving Data

data KindSignature = KindSignature deriving Data

data Kind = ParenK | FunctionK | ApplicationK | InfixApplicationK deriving Data

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

$( concat <$> mapM (\(i,t) -> [d| instance Schema $(return $ ConT t) where
                                   typeId _ = $(return $ LitE $ IntegerL i) |])
     (zip [0..] [ ''Module, ''Declaration, ''Binding, ''Match, ''Alternative, ''Rhs, ''CaseRhs
                , ''Expression, ''Pattern, ''PatternField, ''KindSignature, ''Kind, ''Literal
                , ''Name, ''Operator ])
 )
