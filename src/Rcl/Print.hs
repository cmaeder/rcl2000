module Rcl.Print (ppStmts, prStmt, ppStmt, ppTerm, ppSet, ppVar, ppType,
  Form (..), Format (..), pStmts, rStmt, pSet, Doc, render) where

import Rcl.Ast
import Text.PrettyPrint (Doc, render, text, (<+>), hcat, cat, sep, vcat,
  parens, braces, int)

data Form = Form { format :: Format, prParen :: Bool }

ppStmts :: [Stmt] -> String
ppStmts = render . pStmts form

prStmt :: (Stmt, Vars) -> String
prStmt = render . rStmt form

ppStmt :: Stmt -> String
ppStmt = render . pStmt form

ppTerm :: Term -> String
ppTerm = render . pTerm form

ppSet :: Set -> String
ppSet = render . pSet form

ppVar :: (Var, Set) -> String
ppVar = render . pVar form

form :: Form
form = Form Uni True

ppType :: Maybe SetType -> String
ppType = maybe "Unknown" stSet

pStmts :: Form -> [Stmt] -> Doc
pStmts m = vcat . map (lStmt m)

rStmt :: Form -> (Stmt, Vars) -> Doc
rStmt m (s, vs) = sep [cat . map (pVar m) $ reverse vs, lStmt m s]

pVar :: Form -> (Var, Set) -> Doc
pVar m (i, e) = let f = format m in hcat $ map text [sAll f, stVar i, sIn f]
  ++ [pSet m e, text $ sDot f]

lStmt :: Form -> Stmt -> Doc
lStmt m s = let d = pStmt m s in case format m of
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
  Term b s -> let d = pSet m s in
    if b then hcat [pBar, d, pBar] else d
  EmptySet -> text . sEmpty $ format m
  Num i -> int $ abs i -- avoid negative output

pSet :: Form -> Set -> Doc
pSet m = let f = format m in foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> let p = pBinOp f o in case o of
    Ops -> cat [p, parens $ hcat [d1, text ",", d2]]
    Minus -> cat [pParenSet o s1 d1, hcat [p, braces d2]]
    _ -> sep [pParenSet o s1 d1, p <+> pParenSet o s2 d2]
  , foldUn = \ _ o d -> let b = prParen m
      in (if b || f == LaTeX then cat else sep)
          [pUnOp m o, if b then parens d else d]
  , foldPrim = pPrimSet }

pPrimSet :: Set -> Doc
pPrimSet s = text $ case s of
  PrimSet t -> t
  Var v -> stVar v
  _ -> error "pPrimSet"

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
  Union -> sUnion m
  Inter -> sInter m
  Minus -> "-"
  Ops -> stOps

pBar :: Doc
pBar = text "|"

pUnOp :: Form -> UnOp -> Doc
pUnOp m = text . case format m of
  LaTeX -> (if prParen m then id else (++ "~")) . lUnOp
  _ -> stUnOp
