-- TODO: force completeness with  {-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module Language.Haskell.Heed.Export.Templates where

import Language.Haskell.Heed.Export.Names
import Language.Haskell.Heed.Export.Utilities

import HsExpr
import SrcLoc

exportSplice :: Exporter (Located (HsSplice n))
exportSplice (L l splice) = undefined

exportQuasiQuotation :: Exporter (Located (HsSplice n))
exportQuasiQuotation (L l (HsQuasiQuote _ id l2 str)) = undefined

exportBracket :: Exporter (Located (HsBracket n))
exportBracket (L l bracket) = undefined


-- trfQuasiQuotation' :: TransformName n r => HsSplice n -> Trf (AST.UQuasiQuote (Dom r) RangeStage)
--  -- the lexer does not provide us with tokens '[', '|' and '|]'
-- trfQuasiQuotation' (HsQuasiQuote _ id l str)
--   = AST.UQuasiQuote <$> annLocNoSema quoterLoc (trfName' id)
--                     <*> annLocNoSema (pure strLoc) (pure $ AST.QQString (unpackFS str))
--   where -- assume that there are no white spaces ain the head and the end of the quasi quote
--         quoterLoc = do rng <- asks contRange
--                        return $ mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (updateCol (subtract 1) (srcSpanStart l))
--         strLoc = mkSrcSpan (srcSpanStart l) (updateCol (subtract 2) (srcSpanEnd l))
-- trfQuasiQuotation' qq = unhandledElement "quasi quotation" qq
--
-- trfSplice :: TransformName n r => HsSplice n -> Trf (Ann AST.USplice (Dom r) RangeStage)
-- trfSplice spls = do rng <- asks contRange
--                     annLocNoSema (pure $ getSpliceLoc spls `mappend` rng) (trfSplice' spls)
--
-- getSpliceLoc :: HsSplice a -> SrcSpan
-- getSpliceLoc (HsTypedSplice _ _ e) = getLoc e
-- getSpliceLoc (HsUntypedSplice _ _ e) = getLoc e
-- getSpliceLoc (HsQuasiQuote _ _ sp _) = sp
-- getSpliceLoc (HsSpliced _ _) = noSrcSpan
--
-- trfSplice' :: TransformName n r => HsSplice n -> Trf (AST.USplice (Dom r) RangeStage)
-- trfSplice' (HsTypedSplice _ _ expr) = trfSpliceExpr expr
-- trfSplice' (HsUntypedSplice _ _ expr) = trfSpliceExpr expr
-- trfSplice' s = unhandledElement "splice" s
--
-- -- | TODO: easier with splice decoration
-- trfSpliceExpr :: TransformName n r => Located (HsExpr n) -> Trf (AST.USplice (Dom r) RangeStage)
-- trfSpliceExpr expr =
--   do hasDollar <- allTokenLoc AnnThIdSplice
--      hasDoubleDollar <- allTokenLoc AnnThIdTySplice
--      let newSp = case (hasDollar, hasDoubleDollar) of
--                    ([], []) -> getLoc expr
--                    (_, []) -> updateStart (updateCol (+1)) (getLoc expr)
--                    ([], _) -> updateStart (updateCol (+2)) (getLoc expr)
--      case expr of L _ (HsVar (L _ varName)) -> AST.UIdSplice <$> trfName (L newSp varName)
--                   L _ (HsRecFld fldName) -> AST.UIdSplice <$> trfAmbiguousFieldName' newSp fldName
--                   expr -> AST.UParenSplice <$> trfExpr expr
--
-- trfBracket' :: TransformName n r => HsBracket n -> Trf (AST.UBracket (Dom r) RangeStage)
-- trfBracket' (ExpBr expr) = AST.UExprBracket <$> trfExpr expr
-- trfBracket' (TExpBr expr) = AST.UExprBracket <$> trfExpr expr
-- trfBracket' (VarBr isSingle expr)
--   = AST.UExprBracket <$> annLoc createScopeInfo (updateStart (updateCol (if isSingle then (+1) else (+2))) <$> asks contRange)
--       (AST.UVar <$> (annContNoSema (trfName' expr)))
-- trfBracket' (PatBr pat) = AST.UPatternBracket <$> trfPattern pat
-- trfBracket' (DecBrL decls) = AST.UDeclsBracket <$> trfDecls decls
-- trfBracket' (DecBrG decls) = AST.UDeclsBracket <$> trfDeclsGroup decls
-- trfBracket' (TypBr typ) = AST.UTypeBracket <$> trfType typ
