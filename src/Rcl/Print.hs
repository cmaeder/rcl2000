module Rcl.Print (ppStmts, ppStmt, ppSet,
  Form (..), Format (..), form, pStmts, pSet, Doc, render) where

import Rcl.Ast
import Rcl.Fold
import Text.PrettyPrint (Doc, render, vcat, cat, sep, (<+>), hcat,
  text, int, empty, parens, braces)

data Format = Ascii | Uni | LaTeX

data Form = Form { format :: Format, prParen :: Bool }

ppStmts :: [Stmt] -> String
ppStmts = render . pStmts form

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
d1 $++$ d2 = vcat [d1, text "", d2]

vsep :: [Doc] -> Doc
vsep = foldr ($++$) empty

lStmt :: Form -> Stmt -> Doc
lStmt m s = let d = pStmt m s in case format m of
  LaTeX -> hcat [dollar, d, dollar]
  _ -> d

dollar :: Doc
dollar = text "$"

pStmt :: Form -> Stmt -> Doc
pStmt m = let f = format m in foldStmt FoldStmt
  { foldBool = \ _ o d1 d2 ->
    sep [d1, pBoolOp f o <+> d2]
  , foldCmp = \ _ o d1 d2 ->
    sep [d1, pCmpOp f o <+> d2] }
  $ pSet m

pBoolOp :: Format -> BoolOp -> Doc
pBoolOp m o = text $ case o of
  And -> case m of
    LaTeX -> lAnd
    Uni -> [chAnd]
    Ascii -> stAnd
  Impl -> case m of
    LaTeX -> lImpl
    Uni -> [chImpl]
    Ascii -> stImpl

pCmpOp :: Format -> CmpOp -> Doc
pCmpOp m = text . case m of
  Ascii -> stCmpOp
  Uni -> csCmpOp
  LaTeX -> lCmpOp

pSet :: Form -> Set -> Doc
pSet m = let f = format m in foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> case o of
    Pair -> parens $ hcat [d1, pBinOp f o, d2]
    Minus -> cat [pParenSet o s1 d1, hcat [pBinOp f o, braces d2]]
    _ -> sep [pParenSet o s1 d1, pBinOp f o <+> pParenSet o s2 d2]
  , foldUn = \ (UnOp _ t) o d -> case o of
    Card -> hcat [pBar, d, pBar]
    _ -> let
      b = case t of
        BinOp i _ _ -> i /= Pair
        _ -> prParen m
      c = case f of
        LaTeX -> True
        _ -> case t of
          BinOp Pair _ _ -> True
          _ -> b
      in (if c then cat else sep)
          [pUnOp m { prParen = b } o, if b then parens d else d]
  , foldPrim = pPrimSet f }

pPrimSet :: Format -> Set -> Doc
pPrimSet m s = case s of
  PrimSet t -> text t
  EmptySet -> pEmpty m
  Num i -> int i
  Var i _ -> text $ 'v' : show i
  _ -> error "no prim set"

pParenSet :: BinOp -> Set -> Doc -> Doc
pParenSet o s = case s of
  BinOp i _ _ -> case o of
    Minus -> parens
    Inter -> case i of
      Union -> parens
      _ -> id
    _ -> id
  _ -> id

pBinOp :: Format -> BinOp -> Doc
pBinOp m o = text $ case o of
  Pair -> ","
  Minus -> "-"
  Union -> case m of
    LaTeX -> lUnion
    Uni -> [chUnion]
    Ascii -> stUnion
  Inter -> case m of
    LaTeX -> lInter
    Uni -> [chInter]
    Ascii -> stInter

pBar :: Doc
pBar = text "|"

pUnOp :: Form -> UnOp -> Doc
pUnOp m = text . case format m of
  LaTeX -> (if prParen m then id else (++ "~")) . lUnOp
  _ -> stUnOp

pEmpty :: Format -> Doc
pEmpty m = text $ case m of
  LaTeX -> lEmpty
  Uni -> [chEmpty]
  Ascii -> stEmpty
