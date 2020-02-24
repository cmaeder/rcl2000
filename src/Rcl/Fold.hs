module Rcl.Fold where

import Rcl.Ast (Stmt (..), Term (..), Set (..), BoolOp, CmpOp, BinOp, UnOp)

data FoldSet a = FoldSet
  { foldBin :: Set -> BinOp -> a -> a -> a
  , foldUn :: Set -> UnOp -> a -> a
  , foldPrim :: Set -> a }

mapSet :: FoldSet Set
mapSet = FoldSet
  { foldBin = const BinOp
  , foldUn = const UnOp
  , foldPrim = id }

foldSet :: FoldSet a -> Set -> a
foldSet r s = case s of
  BinOp o s1 s2 -> foldBin r s o (foldSet r s1) $ foldSet r s2
  UnOp o p -> foldUn r s o $ foldSet r p
  _ -> foldPrim r s

data FoldStmt a b = FoldStmt
  { foldBool :: Stmt -> BoolOp -> b -> b -> b
  , foldCmp :: Stmt -> CmpOp -> a -> a -> b }

mapStmt :: FoldStmt Term Stmt
mapStmt = FoldStmt
  { foldBool = const BoolOp
  , foldCmp = const CmpOp }

foldStmt :: FoldStmt a b -> (Term -> a) -> Stmt -> b
foldStmt r f s = case s of
  BoolOp o s1 s2 -> foldBool r s o (foldStmt r f s1) $ foldStmt r f s2
  CmpOp o s1 s2 -> foldCmp r s o (f s1) $ f s2

mapTerm :: (Set -> Set) -> Term -> Term
mapTerm f t = case t of
  Term b s -> Term b $ f s
  _ -> t
