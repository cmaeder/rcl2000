module Rcl.Print (pLaTeX, pAscii, pp) where

import Rcl.Ast
import Text.PrettyPrint

data Form = Ascii | Uni | LaTeX

pLaTeX :: [Stmt] -> String
pLaTeX = render . pStmts LaTeX

pAscii :: [Stmt] -> String
pAscii = render . pStmts Ascii

pp :: [Stmt] -> String
pp = render . pStmts Uni

pStmts :: Form -> [Stmt] -> Doc
pStmts m = (case m of
  LaTeX -> vsep
  _ -> vcat) . map (lStmt m)

($++$) :: Doc -> Doc -> Doc
d1 $++$ d2 = d1 $+$ text "" $+$ d2

vsep :: [Doc] -> Doc
vsep = foldr ($++$) empty

lStmt :: Form -> Stmt -> Doc
lStmt m s = let d = pStmt m s in case m of
  LaTeX -> hcat [dollar, d, dollar]
  _ -> d

dollar :: Doc
dollar = char '$'

pStmt :: Form -> Stmt -> Doc
pStmt m s = case s of
  BoolOp o s1 s2 ->
    sep [pLeftStmt o m s1, pBoolOp m o <+> pRightStmt o m s2]
  CmpOp o s1 s2 ->
    sep [pSet m s1, pCmpOp m o <+> pSet m s2]

pLeftStmt :: BoolOp -> Form -> Stmt -> Doc
pLeftStmt o m s = (case s of
  BoolOp {} -> case o of
    Impl -> parens
    And -> id
  _ -> id) $ pStmt m s

pRightStmt :: BoolOp -> Form -> Stmt -> Doc
pRightStmt o m s = (case s of
  BoolOp i _ _ -> case o of
    Impl -> case i of
      And -> parens
      Impl -> id
    And -> id
  _ -> id) $ pStmt m s

pBoolOp :: Form -> BoolOp -> Doc
pBoolOp m o = text $ case m of
  Ascii -> case o of
    And -> stAnd
    Impl -> stImpl
  Uni -> case o of
    And -> [chAnd]
    Impl -> [chImpl]
  LaTeX -> case o of
    And -> lAnd
    Impl -> lImpl

pCmpOp :: Form -> CmpOp -> Doc
pCmpOp m = text . case m of
  Ascii -> stCmpOp
  Uni -> csCmpOp
  LaTeX -> lCmpOp

pSet :: Form -> Set -> Doc
pSet m s = case s of
  BinOp o s1 s2 ->
    sep [pParenSet o m s1, pBinOp m o <+> pParenSet o m s2]
  UnOp o t -> case o of
     Card -> hcat [pBar, pSet m t, pBar]
     _ -> cat [pUnOp m o, parens (pSet m t)]
  Num i -> int i
  EmptySet -> pEmpty m
  _ -> text (show s)

pParenSet :: BinOp -> Form -> Set -> Doc
pParenSet o m s = (case s of
  BinOp i _ _ -> case o of
    Inter -> case i of
      Union -> parens
      Inter -> id
    Union -> id
  _ -> id) $ pSet m s

pBinOp :: Form -> BinOp -> Doc
pBinOp m o = text $ case m of
  LaTeX -> case o of
    Union -> lUnion
    Inter -> lInter
  Uni -> case o of
    Union -> [chUnion]
    Inter -> [chInter]
  Ascii -> case o of
    Union -> "u"
    Inter -> "n"

pBar :: Doc
pBar = text "|"

pUnOp :: Form -> UnOp -> Doc
pUnOp m = text . case m of
  LaTeX -> lUnOp
  _ -> stUnOp

pEmpty :: Form -> Doc
pEmpty m = text $ case m of
  LaTeX -> lEmpty
  Uni -> [chEmpty]
  Ascii -> "e"
