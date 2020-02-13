module Rcl.Reduce (replaceAO) where

import Rcl.Ast

replaceAO :: Stmt -> Stmt
replaceAO s = case s of
  CmpOp o s1 s2 -> CmpOp o (replAO s1) $ replAO s2
  BoolOp o s1 s2 -> BoolOp o (replaceAO s1) $ replaceAO s2

replAO :: Set -> Set
replAO s = case s of
  BinOp o s1 s2 -> BinOp o (replAO s1) $ replAO s2
  UnOp o r -> let p = replAO r in
    case o of
      AO -> BinOp Minus p (UnOp OE p)
      _ -> UnOp o p
  _ -> s
