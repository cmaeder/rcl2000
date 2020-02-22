module Rcl.Type (typeErrors, wellTyped, typeOfSet, elemType, isElem) where

import Control.Monad (when, unless)
import Control.Monad.State (State, modify, evalState, execState)
import Data.List (find)
import Rcl.Ast
import Rcl.Print (ppStmt, ppSet)

typeErrors :: [Stmt] -> String
typeErrors l = unlines . reverse $ execState (tys l) []

tys :: [Stmt] -> State [String] ()
tys = mapM_ ty

typeOfSet :: Set -> Type
typeOfSet s = evalState (tySet s) []

wellTyped :: Stmt -> Bool
wellTyped s = null $ execState (ty s) []

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
  Elem -> isSet t2 && elemType t2 == t1
  _ -> t1 == NatTy && t1 == t2
    || compatSetTys t1 t2 /= Error && o `elem` [Eq, Ne]

tySet :: Set -> State [String] Type
tySet s = case s of
  BinOp o s1 s2 -> do
    t1 <- tySet s1
    t2 <- tySet s2
    if Error `elem` [t1, t2] then pure Error else case o of
      Pair -> case (t1, t2) of
        (SetTy p1, SetTy p2) -> pure . SetTy $ PairTy p1 p2
        _ -> do
          modify (("expected set components: " ++ ppSet s) :)
          pure Error
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
    when (n < 0) $ modify (("illegal number: " ++ ppSet s) :)
    pure NatTy
  Var (MkVar _ _ t) -> pure t
  PrimSet p -> case find ((== p) . show) primTypes of
    Just b -> pure $ mkSetType b
    Nothing -> case find ((p `elem`) . fst) userTypes of
      Just (_, t) -> pure $ SetTy t
      Nothing -> do
        modify (("unknown base set: " ++ ppSet s) :)
        pure Error

primTypes :: [Base]
primTypes = [U, R, OP, OBJ, P, S]

userTypes :: [([String], SetType)]
userTypes = [(["CU"], Set $ setTy U)
  , (["CP"], Set $ setTy P)
  , (["CR", "read", "write", "AR", "ASR", "SR"], Set $ setTy R)
  , (["GR"], Set . Set $ setTy R)
  , (["RR", "WR", "OWN", "PARENTGRANT", "PARENT", "READ"], setTy R)
  , (["wp", "rp", "OWNAPM", "OWNRPM", "PGPM", "PPM", "RPM"], setTy P)]

compatSetTys :: Type -> Type -> Type
compatSetTys t1 t2 = case (t1, t2) of
  (EmptySetTy, _) | isSet t2 -> t2
  (_, EmptySetTy) | isSet t1 -> t1
  _ | isSet t1 && t1 == t2 -> t1
  _ -> case (mElemOrSetOf t1, mElemOrSetOf t2) of
    (Just s1, Just s2) | s1 == s2 -> mkSetType s1 -- treat elements as sets
    _ -> Error

tyAppl :: UnOp -> Type -> Type
tyAppl o t = case o of
  Card | isSet t || t == EmptySetTy -> NatTy
  OE -> elemType t
  AO | isSet t -> t
  User -> case t of
    SetTy (ElemTy S) -> SetTy $ ElemTy U -- S -> U
    _ | isElemOrSetOf R t -> mkSetType U  -- R -> 2^U
    _ -> Error
  Roles _ | any (`isElemOrSetOf` t) [U, P, S]
    -> mkSetType R -- U + P + S -> 2^R
  Sessions | isElemOrSetOf U t -> mkSetType S  -- U -> 2^S
  Permissions _ | isElemOrSetOf R t -> mkSetType P -- R -> 2^P
  Operations -> case t of
    SetTy (PairTy l r) | isElemOrSet R l && isElemOrSet OBJ r
      -> mkSetType OP -- R x OBJ -> 2^OP
    _ -> Error
  Objects | isElemOrSetOf P t -> mkSetType OBJ  -- P -> 2^OBJ
  _ -> Error

isElemOrSetOf :: Base -> Type -> Bool
isElemOrSetOf s = (== Just s) . mElemOrSetOf

mElemOrSetOf :: Type -> Maybe Base
mElemOrSetOf t = case t of
  SetTy s -> mElemOrSet s
  _ -> Nothing

isElemOrSet :: Base -> SetType -> Bool
isElemOrSet s = (== Just s) . mElemOrSet

mElemOrSet :: SetType -> Maybe Base
mElemOrSet t = case t of
  ElemTy i -> Just i
  Set (ElemTy i) -> Just i -- p 215 "general notation device"
  _ -> Nothing

setTy :: Base -> SetType
setTy = Set . ElemTy

mkSetType :: Base -> Type
mkSetType = SetTy . setTy

isSet :: Type -> Bool
isSet = (/= Error) . elemType

elemType :: Type -> Type
elemType t = case t of
  SetTy (Set s) -> SetTy s
  _ -> Error

isElem :: Type -> Bool
isElem t = case t of
  SetTy (ElemTy _) -> True
  _ -> False
