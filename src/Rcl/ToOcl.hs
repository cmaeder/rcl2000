module Rcl.ToOcl (ocl) where

import Rcl.Ast
import Rcl.Fold
import Rcl.Print (pSet, form)
import Rcl.Reduce (runReduce, Vars)
import Rcl.Type (wellTyped, typeOfSet, elemType, isElem)
import Text.PrettyPrint (Doc, render, text, (<+>), hcat, cat, sep,
  parens, braces, int)

ocl :: [Stmt] -> String
ocl = unlines . zipWith (\ n s -> render $ hcat
    [ text $ "inv i" ++ show n ++ ": "
    , uncurry toOcl $ runReduce s]) [1 :: Int ..]
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
      _ -> sep [singleTerm s1 d1, pCmpOp o <+> singleTerm s2 d2] } termToOcl

parenStmt :: Stmt -> Doc -> Doc
parenStmt s = case s of
  BoolOp Impl _ _ -> parens
  _ -> id

termToOcl :: Term -> Doc
termToOcl t = case t of
  Term b s -> let d = setToOcl s in
    if b then hcat [d, arr, text "size"] else d
  EmptySet -> text "Set{}" -- never possible see isEmpty and notEmpty
  Num i -> int i

singleTerm :: Term -> Doc -> Doc
singleTerm t = case t of
  Term False s -> singleSet s
  _ -> id

setToOcl :: Set -> Doc
setToOcl = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> let p = pBinOp o in case o of
      Ops -> cat [p, parens
        $ hcat [singleSet s1 d1, text ",", singleSet s2 d2]]
      Minus -> cat [hcat [d1, arr, p], parens d2]
      _ -> cat
        [hcat [singleSet s1 d1, arr, p], parens $ singleSet s2 d2]
  , foldUn = \ (UnOp _ s) o d ->
        cat [pUnOp (typeOfSet s) o, parens $ singleSet s d]
  , foldPrim = pSet form }

singleSet :: Set -> Doc -> Doc
singleSet = maybe id singleSetType . typeOfSet

singleSetType :: SetType -> Doc -> Doc
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
  Ops -> "ops"

pUnOp :: Maybe SetType -> UnOp -> Doc
pUnOp t o = let u = map (\ c -> if c == '*' then '_' else c) $ stUnOp o
  in text $ case o of
  User -> if t == Just (ElemTy S) then u else "users"
  Roles _ -> case t of
    Just s -> case if isElem s then Just s else elemType s of
      Just (ElemTy r) -> case r of
        U -> 'u' : u
        P -> 'p' : u
        S -> 's' : u
        _ -> u
      _ -> u
    Nothing -> u
  _ -> u
