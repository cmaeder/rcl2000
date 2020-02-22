module Rcl.ToOcl (ocl) where

import Rcl.Ast
import Rcl.Fold
import Rcl.Print (pSet, form)
import Rcl.Reduce (runReduce, Vars)
import Rcl.Type (wellTyped, typeOfSet, elemType, isElem)
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
      _ -> let t = typeOfSet s in
        cat [pUnOp t o, parens $ singleSetType t d]
  , foldPrim = pSet form }

singleSet :: Set -> Doc -> Doc
singleSet = singleSetType . typeOfSet

singleSetType :: Type -> Doc -> Doc
singleSetType t d =
  if isElem t then hcat [text "Set", braces d] else d

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

pUnOp :: Type -> UnOp -> Doc
pUnOp t o = let u = map (\ c -> if c == '*' then '_' else c) $ stUnOp o
  in text $ case o of
  Operations -> "ops"
  User -> if t == SetTy (ElemTy S) then u else "users"
  Roles p -> case if isElem t then t else elemType t of
    SetTy (ElemTy r) -> case r of
      U -> 'u' : u
      P -> 'p' : u
      S -> 's' : u
      _ -> u
    _ -> u
  _ -> u
