module Rcl.Type where

import Control.Monad.State
import Rcl.Ast
import Rcl.Print

data ElementType = Uty | Rty | OPty | OBJty | Pty | Sty deriving Eq

data SetType = ElemTy ElementType | Set SetType deriving Eq

data Type = SetTy SetType | NatTy | BoolTy | EmptySetTy deriving Eq

exec :: [Stmt] -> [String]
exec l = execState (tys l) []

tys :: [Stmt] -> State [String] ()
tys = mapM_ ty

ty :: Stmt -> State [String] Type
ty s = case s of
  BoolOp _ s1 s2 -> do
    t1 <- ty s1
    t2 <- ty s2
    unless (t1 == BoolTy) $ modify (("no bool: " ++ ppStmt s1) :)
    unless (t2 == BoolTy) $ modify (("no bool: " ++ ppStmt s2) :)
    pure BoolTy
  CmpOp o s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    -- check type compatibility
    pure BoolTy

tySet :: Set -> State [String] Type
tySet s = case s of
  BinOp o s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    -- check type compatibility
    pure t1 -- or the better type
  UnOp o s1 -> do
    t1 <- tySet s1
    pure t1 -- depending on o
  _ -> do
    modify (("nyi: " ++ ppSet s) :)
    pure $ SetTy (ElemTy Uty)
