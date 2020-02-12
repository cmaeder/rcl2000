module Rcl.Type where

import Control.Monad.State
import Rcl.Ast
import Rcl.Print

data ElementType = Uty | Rty | OPty | OBJty | Pty | Sty deriving Eq

data SetType = ElemTy ElementType | Set SetType deriving Eq

data Type = SetTy SetType | NatTy | EmptySetTy | Error deriving Eq

exec :: [Stmt] -> [String]
exec l = execState (tys l) []

tys :: [Stmt] -> State [String] ()
tys = mapM_ ty

ty :: Stmt -> State [String] ()
ty s = case s of
  BoolOp _ s1 s2 -> do
    t1 <- ty s1
    t2 <- ty s2
    pure ()
  CmpOp o s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    -- check type compatibility
    pure ()

tySet :: Set -> State [String] Type
tySet s = case s of
  BinOp o s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    -- check type compatibility
    pure t1 -- or the better type
  UnOp o s1 -> do
    t1 <- tySet s1
    case t1 of
      Error -> pure t1 -- was reported earlier
      _ -> do
        let t = tyAppl o t1
        when (t == Error)
          $ modify (("wrongly typed application: " ++ ppSet s) :)
        pure t
  EmptySet -> pure EmptySetTy
  Num _ -> pure NatTy
  _ -> case lookup s bases of
     Just e -> pure . SetTy $ setTy e
     _ -> case lookup s conflicts of
       Just e -> pure . SetTy . Set $ setTy e
       _ -> error $ "unexpected input set: " ++ ppSet s

tyAppl :: UnOp -> Type -> Type
tyAppl o t = case o of
  Card | isSet t -> NatTy
  OE -> elemTy t
  AO | isSet t -> t
  User -> case t of
    SetTy (ElemTy Sty) -> SetTy (ElemTy Uty) -- S -> U
    SetTy (ElemTy Rty) -> SetTy $ setTy Uty  -- R -> 2^U
    _ -> Error
  Roles -> rolesAppl t
  RolesStar -> rolesAppl t
  Sessions -> case t of
    SetTy (ElemTy Uty) -> SetTy $ setTy Sty  -- U -> 2^S
  Permissions -> permAppl t
  PermissionsStar -> permAppl t
  Operations -> case t of
    SetTy (ElemTy e) | elem e [Rty, OBJty] -> -- "R x OBJ" not supported!
      SetTy $ setTy OPty -- R x OBJ -> 2^OP
    _ -> Error
  Object -> case t of
    SetTy (ElemTy Pty) -> SetTy $ setTy OBJty  -- P -> 2^OBJ
    _ -> Error

rolesAppl :: Type -> Type
rolesAppl t = case t of
  SetTy (ElemTy e) | elem e [Uty, Pty, Sty] ->
    SetTy $ setTy Rty -- U + P + S -> 2^R
  _ -> Error

permAppl :: Type -> Type
permAppl t = case t of
  SetTy (ElemTy Rty) -> SetTy $ setTy Pty -- R -> 2^P
  _ -> Error

isSet :: Type -> Bool
isSet = (/= Error) . elemTy

elemTy :: Type -> Type
elemTy t = case t of
  SetTy (Set s) -> SetTy s
  _ -> Error

setTy :: ElementType -> SetType
setTy = Set . ElemTy

bases :: [(Set, ElementType)]
bases = [(U, Uty), (R, Rty), (OP, OPty), (OBJ, OBJty), (P, Pty), (S, Sty)]

conflicts :: [(Set, ElementType)]
conflicts = [(CR, Rty), (CU, Uty), (CP, Pty)]
