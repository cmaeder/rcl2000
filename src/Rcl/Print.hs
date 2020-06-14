module Rcl.Print (ppStmts, prStmt, ppStmt, ppTerm, ppSet, ppVar,
  Form (..), Format (..), pStmts, rStmt, pSet, Doc, render, lineStmt) where

import Rcl.Ast
import Text.PrettyPrint

data Form = Form { format :: Format, prParen :: Bool }

ppStmts :: [Let] -> String
ppStmts = render . pStmts form

prStmt :: (Stmt, Vars) -> String
prStmt = render . rStmt form

ppStmt :: Stmt -> String
ppStmt = render . pStmt form

lineStmt :: Let -> String
lineStmt = renderStyle style {mode = OneLineMode} . pLet form

ppTerm :: Term -> String
ppTerm = render . pTerm form

ppSet :: Set -> String
ppSet = render . pSet form

ppVar :: (Var, Set) -> String
ppVar = render . pVar form

form :: Form
form = Form Uni True

pStmts :: Form -> [Let] -> Doc
pStmts m = vcat . map (lLet m)

rStmt :: Form -> (Stmt, Vars) -> Doc
rStmt m (s, vs) = sep [cat . map (pVar m) $ reverse vs, pStmt m s]

pVar :: Form -> (Var, Set) -> Doc
pVar m (i, e) = let f = format m in hcat $ map text [sAll f, stVar i, sIn f]
  ++ [pSet m e, text $ sDot f]

pLet :: Form -> Let -> Doc
pLet m (Let as s) = let d = pStmt m s in
  if null as then d else
    hsep $ text "let" : punctuate semi (map (pAss m) as) ++ [text "in", d]

pAss :: Form -> (String, Set) -> Doc
pAss m (s, r) = hsep [text $ s ++ " =", pSet m r]

lLet :: Form -> Let -> Doc
lLet m s = let d = pLet m s in case format m of
  LaTeX -> hcat [dollar, d, dollar, text "\n"]
  _ -> d

dollar :: Doc
dollar = text "$"

pStmt :: Form -> Stmt -> Doc
pStmt m = let f = format m in foldStmt FoldStmt
  { foldBool = \ _ o d1 d2 ->
    sep [d1, pBoolOp f o <+> d2]
  , foldCmp = \ _ o d1 d2 ->
    sep [d1, pCmpOp f o <+> d2] }
  $ pTerm m

pBoolOp :: Format -> BoolOp -> Doc
pBoolOp m o = text $ case o of
  And -> sAnd m
  Impl -> sImpl m

pCmpOp :: Format -> CmpOp -> Doc
pCmpOp m = text . sCmpOp m

pTerm :: Form -> Term -> Doc
pTerm m t = case t of
  Term b s -> let d = pSet m s in case b of
    Card -> hcat [pBar, d, pBar]
    TheSet -> d
  EmptySet -> text . sEmpty $ format m
  Num i -> int i

pSet :: Form -> Set -> Doc
pSet m = let f = format m in foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> let
      p = pBinOp f o
      a1 = pParenSet o s1 d1
      a2 = pParenSet o s2 d2
      in case o of
    Operations _ -> cat [p, parens $ hcat [d1, text ",", d2]]
    Minus -> cat [a1, hcat [p, a2]]
    _ -> sep [a1, p <+> a2]
  , foldUn = \ (UnOp _ s) o d -> case o of
      Typed ex ts -> case ex of
        Derived -> d
        Explicit -> cat [case untyped s of
            PrimSet _ -> d
            _ -> parens d
          , text $ ':' : ppType ts]
      _ -> let b = prParen m in (if b || f == LaTeX then cat else sep)
          [pUnOp m o, if b then parens d else d]
  , foldBraced = \ _ ds -> braces . fsep $ punctuate comma ds
  , foldPrim = pPrimSet }

pPrimSet :: Set -> Doc
pPrimSet s = text $ case s of
  PrimSet t -> t
  Var v -> stVar v
  _ -> error "pPrimSet"

pParenSet :: BinOp -> Set -> Doc -> Doc
pParenSet o s = case untyped s of
  BinOp i _ _ -> case o of
    Minus -> parens
    Inter -> case i of
      Union -> parens
      _ -> id
    _ -> id
  _ -> id

pBinOp :: Format -> BinOp -> Doc
pBinOp m o = text $ case o of
  Union -> sUnion m
  Inter -> sInter m
  Minus -> "-"
  _ -> case m of
    LaTeX -> lUnOp o
    _ -> stUnOp o

pBar :: Doc
pBar = text "|"

pUnOp :: Form -> UnOp -> Doc
pUnOp m = text . case format m of
  LaTeX -> (if prParen m then id else (++ "~")) . lUnOp
  _ -> stUnOp
