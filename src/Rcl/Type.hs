module Rcl.Type where

import Control.Monad.State
import Data.List (find, isSuffixOf)
import Rcl.Ast
import Rcl.Print

exec :: [Stmt] -> String
exec l = unlines . reverse $ execState (tys l) []

tys :: [Stmt] -> State [String] ()
tys = mapM_ ty

typeOfSet :: Set -> Type
typeOfSet s = evalState (tySet s) []

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
  BinOp o s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    if Error `elem` [t1, t2] then pure Error else case o of
      Minus -> do
        unless (compatCmp Elem t2 t1)
          $ modify (("wrongly typed set minus element: " ++ ppSet s) :)
        pure t1
      _ -> do
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
  Var _ t -> pure t
  PrimSet p -> case find (`isSuffixOf` p) primTypes of
    Just b -> if b == p then pure $ mkType p
      else pure . SetTy . Set $ setTy b
      -- assume the base type is a suffix of a nested set like CR, CU, CP
    _ -> do
     modify (("unknown base set: " ++ ppSet s) :)
     pure Error

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
    SetTy (ElemTy "S") -> SetTy (ElemTy "U") -- S -> U
    _ | isElemOrSetOf "R" t -> mkType "U"  -- R -> 2^U
    _ -> Error
  Roles -> rolesAppl t
  RolesStar -> rolesAppl t
  Sessions | isElemOrSetOf "U" t -> mkType "S"  -- U -> 2^S
  Permissions -> permAppl t
  PermissionsStar -> permAppl t
  Operations | any (`isElemOrSetOf` t) ["R", "OBJ"]
    -- "R x OBJ" not supported!
    -> SetTy $ ElemTy "OP" -- R x OBJ -> 2^OP
  Object | isElemOrSetOf "P" t -> mkType "OBJ"  -- P -> 2^OBJ
  _ -> Error

rolesAppl :: Type -> Type
rolesAppl t = if any (`isElemOrSetOf` t) $ map (: []) "UPS"
  then mkType "R" -- U + P + S -> 2^R
  else Error

permAppl :: Type -> Type
permAppl t = if isElemOrSetOf "R" t then mkType "P" -- R -> 2^P
  else Error

isElemOrSetOf :: String -> Type -> Bool
isElemOrSetOf e t = case t of
  SetTy (ElemTy i) | i == e -> True
  SetTy (Set (ElemTy i)) | i == e -> True -- p 215 "general notation device"
  _ -> False

setTy :: String -> SetType
setTy = Set . ElemTy

mkType :: String -> Type
mkType = SetTy . setTy

isSet :: Type -> Bool
isSet = (/= Error) . elemTy

elemTy :: Type -> Type
elemTy t = case t of
  SetTy (Set s) -> SetTy s
  _ -> Error
