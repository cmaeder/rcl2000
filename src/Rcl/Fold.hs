module Rcl.Fold where

import Rcl.Ast (Stmt (..), Set (..), BoolOp, CmpOp, BinOp, UnOp)

data FoldSet a = FoldSet
  { foldBin :: Set -> BinOp -> a -> a -> a
  , foldUn :: Set -> UnOp -> a -> a
  , foldPrim :: Set -> a }

mapSet :: FoldSet Set
mapSet = FoldSet
  { foldBin = const BinOp
  , foldUn = const UnOp
  , foldPrim = id }

const2 :: a -> b -> c -> a
const2 = const . const

constSet :: (a -> a -> a) -> a -> FoldSet a
constSet f a = FoldSet
  { foldBin = const2 f
  , foldUn = const2 id
  , foldPrim = const a }

foldSet :: FoldSet a -> Set -> a
foldSet r s = case s of
  BinOp o s1 s2 -> foldBin r s o (foldSet r s1) $ foldSet r s2
  UnOp o p -> foldUn r s o $ foldSet r p
  _ -> foldPrim r s

data FoldStmt a b = FoldStmt
  { foldBool :: Stmt -> BoolOp -> b -> b -> b
  , foldCmp :: Stmt -> CmpOp -> a -> a -> b }

mapStmt :: FoldStmt Set Stmt
mapStmt = FoldStmt
  { foldBool = const BoolOp
  , foldCmp = const CmpOp }

constStmt :: (b -> b -> b) -> (a -> a -> b) -> FoldStmt a b
constStmt f g = FoldStmt
  { foldBool = const2 f
  , foldCmp = const2 g }

foldStmt :: FoldStmt a b -> (Set -> a) -> Stmt -> b
foldStmt r f s = case s of
  BoolOp o s1 s2 -> foldBool r s o (foldStmt r f s1) $ foldStmt r f s2
  CmpOp o s1 s2 -> foldCmp r s o (f s1) $ f s2
