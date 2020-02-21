module Rcl.ToOcl (ocl) where

import Rcl.Ast
import Rcl.Fold
import Rcl.Print (pSet, form)
import Rcl.Reduce (runReduce, Vars)
import Rcl.Type (wellTyped, typeOfSet, isElem)
import Text.PrettyPrint (Doc, render, text, (<+>), hcat, cat, sep,
  parens, braces)

ocl :: [Stmt] -> String
ocl = unlines . map (\ (n, s) -> render $ hcat
    [ text $ "inv i" ++ show n ++ ": "
    , uncurry toOcl $ runReduce s]) . zip [1 :: Int ..]
    . filter wellTyped

toOcl :: Stmt -> Vars -> Doc
toOcl = foldl (\ f (i, s) -> cat
   [ hcat [setToOcl s, arr, text "forAll"]
   , parens $ sep [text $ stVar i ++ " |", f]]) . stmtToOcl

arr :: Doc
arr = text "->"

stmtToOcl :: Stmt -> Doc
stmtToOcl = foldStmt FoldStmt
  { foldBool = \ (BoolOp _ s1 s2) o d1 d2 ->
      sep [parenStmt s1 d1, pBoolOp o <+> parenStmt s2 d2]
  , foldCmp = \ (CmpOp _ s1 s2) o d1 d2 -> case o of
      Elem -> cat [hcat [d2, arr, text "includes"], parens d1]
      Eq | s2 == EmptySet -> hcat [d1, arr, text "isEmpty"]
      Ne | s2 == EmptySet -> hcat [d1, arr, text "notEmpty"]
      _ -> sep [singleSet s1 d1, pCmpOp o <+> singleSet s2 d2] } setToOcl

parenStmt :: Stmt -> Doc -> Doc
parenStmt s = case s of
  BoolOp Impl _ _ -> parens
  _ -> id

setToOcl :: Set -> Doc
setToOcl = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> case o of
      Pair -> cat [hcat [singleSet s1 d1, pBinOp o], singleSet s2 d2]
      Minus -> cat [hcat [d1, arr, pBinOp o], parens d2]
      _ -> cat
        [hcat [singleSet s1 d1, arr, pBinOp o], parens $ singleSet s2 d2]
  , foldUn = \ (UnOp _ s) o d -> case o of
      Card -> hcat [d, arr, text "size"]
      _ -> cat [pUnOp s o, parens $ singleSet s d]
  , foldPrim = pSet form }

singleSet :: Set -> Doc -> Doc
singleSet s d =
  if isElem $ typeOfSet s then hcat [text "Set", braces d] else d

pBoolOp :: BoolOp -> Doc
pBoolOp o = text $ case o of
  And -> "and"
  Impl -> "implies"

pCmpOp :: CmpOp -> Doc
pCmpOp o = text $ case o of
  Ne -> "<>"
  _ -> stCmpOp o

pBinOp :: BinOp -> Doc
pBinOp o = text $ case o of
  Union -> "union"
  Inter -> "intersection"
  Minus -> "excluding"
  Pair -> ","

pUnOp :: Set -> UnOp -> Doc
pUnOp s o = text $ case o of
  Operations -> "ops"
  User -> if typeOfSet s == SetTy (ElemTy "S") then "user" else "users"
  _ -> map (\ c -> if c == '*' then '_' else c) $ stUnOp o
