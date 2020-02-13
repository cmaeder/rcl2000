module Rcl.Print (pLaTeX, pAscii, pp, ppStmt, ppSet) where

import Rcl.Ast
import Text.PrettyPrint

data Format = Ascii | Uni | LaTeX

data Form = Form { format :: Format, prParen :: Bool }

pLaTeX :: Bool -> [Stmt] -> String
pLaTeX b = render . pStmts (Form LaTeX b)

pAscii :: Bool -> [Stmt] -> String
pAscii b = render . pStmts (Form Ascii b)

pp :: Bool -> [Stmt] -> String
pp b = render . pStmts (Form Uni b)

ppStmt :: Stmt -> String
ppStmt = render . pStmt form

ppSet :: Set -> String
ppSet = render . pSet form

form :: Form
form = Form Uni True

pStmts :: Form -> [Stmt] -> Doc
pStmts m = (case format m of
  LaTeX -> vsep
  _ -> vcat) . map (lStmt m)

($++$) :: Doc -> Doc -> Doc
d1 $++$ d2 = d1 $+$ text "" $+$ d2

vsep :: [Doc] -> Doc
vsep = foldr ($++$) empty

lStmt :: Form -> Stmt -> Doc
lStmt m s = let d = pStmt m s in case format m of
  LaTeX -> hcat [dollar, d, dollar]
  _ -> d

dollar :: Doc
dollar = char '$'

pStmt :: Form -> Stmt -> Doc
pStmt m s = case s of
  BoolOp o s1 s2 ->
    sep [pLeftStmt o m s1, pBoolOp m o <+> pRightStmt o m s2]
    -- the original input language does not allow parentheses!
  CmpOp o s1 s2 ->
    sep [pSet m s1, pCmpOp m o <+> pSet m s2]

pLeftStmt :: BoolOp -> Form -> Stmt -> Doc
pLeftStmt o m s = (case s of
  BoolOp {} -> case o of
    Impl -> parens  -- Impl binds stronger than And and is not nested
    And -> id -- we have only top-level Ands without parens
  _ -> id) $ pStmt m s

pRightStmt :: BoolOp -> Form -> Stmt -> Doc
pRightStmt o m s = (case s of
  BoolOp i _ _ -> case o of
    Impl -> case i of
      And -> parens -- as above
      Impl -> id  -- Impl is usually right associative if nested
    And -> id
  _ -> id) $ pStmt m s

pBoolOp :: Form -> BoolOp -> Doc
pBoolOp m o = text $ case format m of
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
pCmpOp m = text . case format m of
  Ascii -> stCmpOp
  Uni -> csCmpOp
  LaTeX -> lCmpOp

pSet :: Form -> Set -> Doc
pSet m s = case s of
  BinOp o s1 s2 -> case o of
    Minus -> cat [pParenSet o m s1, hcat [pBinOp m o, braces $ pSet m s2]]
    _ -> sep [pParenSet o m s1, pBinOp m o <+> pParenSet o m s2]
  UnOp o t -> case o of
     Card -> hcat [pBar, pSet m t, pBar]
     _ -> let
       b = case t of
         BinOp {} -> True
         _ -> prParen m
       c = case format m of
         LaTeX -> True
         _ -> b
       d = pSet m t
       in (if c then cat else sep)
          [pUnOp m { prParen = b } o, if b then parens d else d]
  Num i -> int i
  EmptySet -> pEmpty m
  Var i -> text $ 'v' : show i
  _ -> text (show s)

pParenSet :: BinOp -> Form -> Set -> Doc
pParenSet o m s = (case s of
  BinOp i _ _ -> case o of
    Minus -> parens
    Inter -> case i of
      Union -> parens
      _ -> id
    Union -> id
  _ -> id) $ pSet m s

pBinOp :: Form -> BinOp -> Doc
pBinOp m o = text $ case format m of
  LaTeX -> case o of
    Union -> lUnion
    Inter -> lInter
    Minus -> "-"
  Uni -> case o of
    Union -> [chUnion]
    Inter -> [chInter]
    Minus -> "-"
  Ascii -> case o of
    Union -> stUnion
    Inter -> stInter
    Minus -> "-"

pBar :: Doc
pBar = text "|"

pUnOp :: Form -> UnOp -> Doc
pUnOp m = text . case format m of
  LaTeX -> (if prParen m then id else (++ "~")) . lUnOp
  _ -> stUnOp

pEmpty :: Form -> Doc
pEmpty m = text $ case format m of
  LaTeX -> lEmpty
  Uni -> [chEmpty]
  Ascii -> stEmpty
