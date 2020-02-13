module Rcl.Type where

import Control.Monad.State
import Rcl.Ast
import Rcl.Print

data ElementType = Uty | Rty | OPty | OBJty | Pty | Sty deriving Eq

data SetType = ElemTy ElementType | Set SetType deriving Eq

data Type = SetTy SetType | NatTy | EmptySetTy | Error deriving Eq

exec :: [Stmt] -> String
exec l = unlines . reverse $ execState (tys l) []

tys :: [Stmt] -> State [String] ()
tys = mapM_ ty

ty :: Stmt -> State [String] ()
ty s = case s of
  BoolOp _ s1 s2 -> ty s1 >> ty s2
  CmpOp o s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    unless (Error `elem` [t1, t2] || compatCmp o t1 t2)
      $ modify (("wrongly typed relation: " ++ ppStmt s) :)

compatCmp :: CmpOp -> Type -> Type -> Bool
compatCmp o t1 t2 = case o of
  Elem -> isSet t2 && elemTy t2 == t1
  _ -> t1 == NatTy && t1 == t2
    || compatSetTys t1 t2 /= Error && o `elem` [Eq, Ne]

tySet :: Set -> State [String] Type
tySet s = case s of
  BinOp _ s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    if Error `elem` [t1, t2] then pure Error else do
      let t = compatSetTys t1 t2
      when (t == Error)
          $ modify (("wrongly typed set operation: " ++ ppSet s) :)
      pure t
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
  Num n -> do
    unless (n > 0) $ modify (("illegal number: " ++ ppSet s) :)
    pure NatTy
  _ -> case lookup s bases of
     Just e -> pure . SetTy $ setTy e
     _ -> case lookup s conflicts of
       Just e -> pure . SetTy . Set $ setTy e
       _ -> error $ "unexpected input set: " ++ ppSet s

compatSetTys :: Type -> Type -> Type
compatSetTys t1 t2 = case (t1, t2) of
  (EmptySetTy, _) | isSet t2 -> t2
  (_, EmptySetTy) | isSet t1 -> t1
  _ | isSet t1 && t1 == t2 -> t1
  _ -> Error

tyAppl :: UnOp -> Type -> Type
tyAppl o t = case o of
  Card | isSet t || t == EmptySetTy -> NatTy
  OE -> elemTy t
  AO | isSet t -> t
  User -> case t of
    SetTy (ElemTy Sty) -> SetTy (ElemTy Uty) -- S -> U
    _ | isElemOrSetOf Rty t -> SetTy $ setTy Uty  -- R -> 2^U
    _ -> Error
  Roles -> rolesAppl t
  RolesStar -> rolesAppl t
  Sessions | isElemOrSetOf Uty t -> SetTy $ setTy Sty  -- U -> 2^S
  Permissions -> permAppl t
  PermissionsStar -> permAppl t
  Operations | any (`isElemOrSetOf` t) [Rty, OBJty]
    -- "R x OBJ" not supported!
    -> SetTy $ setTy OPty -- R x OBJ -> 2^OP
  Object | isElemOrSetOf Pty t -> SetTy $ setTy OBJty  -- P -> 2^OBJ
  _ -> Error

rolesAppl :: Type -> Type
rolesAppl t = if any (`isElemOrSetOf` t) [Uty, Pty, Sty]
  then SetTy $ setTy Rty -- U + P + S -> 2^R
  else Error

permAppl :: Type -> Type
permAppl t = if isElemOrSetOf Rty t then SetTy $ setTy Pty -- R -> 2^P
  else Error

isElemOrSetOf :: ElementType -> Type -> Bool
isElemOrSetOf e t = case t of
  SetTy (ElemTy i) | i == e -> True
  SetTy (Set (ElemTy i)) | i == e -> True
  _ -> False

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
