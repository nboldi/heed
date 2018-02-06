{-# LANGUAGE DeriveGeneric #-}
module Language.Haskell.Heed.TypeRepresentation where

import Language.Haskell.Heed.DBUtils
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.List
import Data.Binary
import GHC.Generics (Generic)

data TypeRepresentation = TRForAll [String] TypeRepresentation
                        | TRVar String
                        | TRFun [TypeRepresentation] TypeRepresentation
                        | TRApp TypeRepresentation TypeRepresentation
                        | TRConApp String [TypeRepresentation]
                        | TRNumLit Integer
                        | TRStringLit String
                        | TRCoercion
  deriving (Show, Read, Generic)

instance Binary TypeRepresentation

typeRepEq :: TypeRepresentation -> TypeRepresentation -> Bool
typeRepEq = compareTypeRep []

-- TODO: use ByteString for storing types when there will be support in Selda

readTypeRep :: Text -> TypeRepresentation
readTypeRep = decode . BL.fromStrict . textToBS

writeTypeRep :: TypeRepresentation -> Text
writeTypeRep = bsToText . BL.toStrict . encode

compareTypeRep :: [(String,String)] -> TypeRepresentation -> TypeRepresentation -> Bool
compareTypeRep env (TRForAll vars1 t1) (TRForAll vars2 t2)
  = compareTypeRep (env ++ zip vars1 vars2) t1 t2
compareTypeRep env (TRVar v1) (TRVar v2)
  = case find ((==v1) . fst) env of Just (v1',v2') -> v2 == v2'
                                    Nothing -> isNothing (find ((==v2) . snd) env) && v1 == v2
compareTypeRep env (TRFun args1 res1) (TRFun args2 res2)
  = and (zipWith (compareTypeRep env) args1 args2) && compareTypeRep env res1 res2
compareTypeRep env (TRApp base1 arg1) (TRApp base2 arg2)
  = compareTypeRep env base1 base2 && compareTypeRep env arg1 arg2
compareTypeRep env (TRConApp base1 args1) (TRConApp base2 args2)
  = base1 == base2 && and (zipWith (compareTypeRep env) args1 args2)
compareTypeRep _ (TRNumLit i1) (TRNumLit i2) = i1 == i2
compareTypeRep _ (TRStringLit s1) (TRStringLit s2) = s1 == s2
compareTypeRep _ TRCoercion TRCoercion = True
compareTypeRep _ _ _ = False