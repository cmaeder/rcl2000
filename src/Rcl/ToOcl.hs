module Rcl.ToOcl (ocl) where

import Rcl.Ast
import Rcl.Reduce (runReduce, Vars)
import Rcl.Type (wellTyped, typeOfSet, elemType, isElem)
import Text.PrettyPrint (Doc, render, text, (<+>), hcat, cat, sep,
  parens, braces, int)

ocl :: UserTypes -> [Stmt] -> String
ocl us = unlines . zipWith (\ n s -> render $ hcat
    [ text $ "inv i" ++ show n ++ ": "
    , uncurry (toOcl us) $ runReduce us s]) [1 :: Int ..]
    . filter (wellTyped us)

toOcl :: UserTypes -> Stmt -> Vars -> Doc
toOcl us = foldl (\ f (i, s) -> cat
   [ hcat [setToOcl us s, arr, text "forAll"]
   , parens $ sep [text $ stVar i ++ " |", f]]) . stmtToOcl us

arr :: Doc
arr = text "->"

stmtToOcl :: UserTypes -> Stmt -> Doc
stmtToOcl us = foldStmt FoldStmt
  { foldBool = \ (BoolOp _ s1 s2) o d1 d2 ->
      sep [parenStmt s1 d1, pBoolOp o <+> parenStmt s2 d2]
  , foldCmp = \ (CmpOp _ s1 s2) o d1 d2 -> case o of
      Elem -> cat [hcat [d2, arr, text "includes"], parens d1]
      Eq | s2 == EmptySet -> hcat [d1, arr, text "isEmpty"]
      Ne | s2 == EmptySet -> hcat [d1, arr, text "notEmpty"]
      _ -> sep [singleTerm us s1 d1, pCmpOp o <+> singleTerm us s2 d2] }
  $ termToOcl us

parenStmt :: Stmt -> Doc -> Doc
parenStmt s = case s of
  BoolOp Impl _ _ -> parens
  _ -> id

termToOcl :: UserTypes -> Term -> Doc
termToOcl us t = case t of
  Term b s -> let d = setToOcl us s in
    if b then hcat [d, arr, text "size"] else d
  EmptySet -> text "Set{}" -- never possible see isEmpty and notEmpty
  Num i -> int i

singleTerm :: UserTypes -> Term -> Doc -> Doc
singleTerm us t = case t of
  Term False s -> singleSet us s
  _ -> id

setToOcl :: UserTypes -> Set -> Doc
setToOcl us = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> let
      p = pBinOp o
      a1 = singleSet us s1 d1
      a2 = singleSet us s2 d2
      in case o of
      Ops -> cat [p, parens $ hcat [a1, text ",", a2]]
      Minus -> cat [hcat [d1, arr, p], parens d2]
      _ -> cat [hcat [a1, arr, p], parens a2]
  , foldUn = \ (UnOp _ s) o d ->
        cat [pUnOp (typeOfSet us s) o, parens $ singleSet us s d]
  , foldPrim = \ s -> text $ case s of
      PrimSet t -> t ++ "()"
      Var (MkVar i t _) -> t ++ show i
      _ -> error "setToOcl: no prim set" }

singleSet :: UserTypes -> Set -> Doc -> Doc
singleSet us = maybe id singleSetType . typeOfSet us

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
  Roles _ -> case t >>= \ s -> if isElem s then Just s else elemType s of
      Just (ElemTy r) -> case r of
        U -> 'u' : u
        P -> 'p' : u
        S -> 's' : u
        _ -> u
      _ -> u
  _ -> u
