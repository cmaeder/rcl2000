module Rcl.Type (typeErrors, wellTyped, typeOfSet, elemType, isElem) where

import Control.Monad (when, unless)
import Control.Monad.State (State, modify, evalState, execState)
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import Rcl.Ast
import Rcl.Print (ppStmt, ppSet)

typeErrors :: [Stmt] -> String
typeErrors l = unlines . reverse $ execState (tys l) []

tys :: [Stmt] -> State [String] ()
tys = mapM_ ty

typeOfSet :: Set -> Maybe SetType
typeOfSet s = evalState (tySet s) []

wellTyped :: Stmt -> Bool
wellTyped s = null $ execState (ty s) []

ty :: Stmt -> State [String] ()
ty s = case s of
  BoolOp _ s1 s2 -> ty s1 >> ty s2
  CmpOp o e1 e2 -> do
    m1 <- tyTerm e1
    m2 <- tyTerm e2
    let md = modify (("wrongly typed relation: " ++ ppStmt s) :)
    case (m1, m2) of
      (Just t1, Just t2) -> case (t1, t2) of
        (SetTy s1, SetTy s2) ->
          unless (elemType s2 == Just s1 && o == Elem
            || o `elem` [Eq, Ne] && isJust (compatSetTys s1 s2)) md
        (SetTy _, EmptySetTy) -> pure ()
        (NatTy, NatTy) | o /= Elem -> pure ()
        _ -> md
      _ -> pure () -- error reported earlier

tyTerm :: Term -> State [String] (Maybe Type)
tyTerm t = case t of
  Term b s -> do
    m <- tySet s
    pure $ if b then Just NatTy else fmap SetTy m
  EmptySet -> pure $ Just EmptySetTy
  Num n -> do
    when (n < 0) $ modify (("illegal number: " ++ show n) :)
    pure $ Just NatTy

tySet :: Set -> State [String] (Maybe SetType)
tySet s = case s of
  BinOp o s1 s2 -> do
    m1 <- tySet s1
    m2 <- tySet s2
    case (m1, m2) of
      (Just t1, Just t2) -> case o of
        Ops -> do
          unless (isElemOrSet R t1 && isElemOrSet OBJ t2)
            $ modify (("expected role and object arguments: " ++ ppSet s) :)
          pure $ mkSetType OP -- R x OBJ -> 2^OP
        Minus -> do
          unless (elemType t1 == Just t2)
            $ modify (("wrongly typed set minus element: " ++ ppSet s) :)
          pure $ Just t1
        _ -> do
          let t = compatSetTys t1 t2
          when (isNothing t)
            $ modify (("wrongly typed set operation: " ++ ppSet s) :)
          pure t
      _ -> pure Nothing
  UnOp o s1 -> do
    t1 <- tySet s1
    case t1 of
      Nothing -> pure t1 -- was reported earlier
      Just st -> do
        let t = tyAppl o st
        when (isNothing t)
          $ modify (("wrongly typed application: " ++ ppSet s) :)
        pure t
  Var (MkVar _ _ t) -> pure t
  PrimSet p -> case find ((== p) . show) primTypes of
    Just b -> pure $ mkSetType b
    Nothing -> case find ((p `elem`) . fst) userTypes of
      Just (_, t) -> pure $ Just t
      Nothing -> do
        modify (("unknown base set: " ++ ppSet s) :)
        pure Nothing

primTypes :: [Base]
primTypes = [U, R, OP, OBJ, P, S]

userTypes :: [([String], SetType)]
userTypes = [(["CU"], Set $ setTy U)
  , (["CP"], Set $ setTy P)
  , (["CR", "read", "write", "AR", "ASR", "SR"], Set $ setTy R)
  , (["GR"], Set . Set $ setTy R)
  , (["RR", "WR", "OWN", "PARENTGRANT", "PARENT", "READ"], setTy R)
  , (["wp", "rp", "OWNAPM", "OWNRPM", "PGPM", "PPM", "RPM"], setTy P)]

compatSetTys :: SetType -> SetType -> Maybe SetType
compatSetTys t1 t2 = case (mElemOrSet t1, mElemOrSet t2) of
    (Just s1, Just s2) | s1 == s2 -> mkSetType s1 -- treat elements as sets
    _ -> Nothing

tyAppl :: UnOp -> SetType -> Maybe SetType
tyAppl o t = case o of
  OE -> elemType t
  AO | isSet t -> Just t
  User -> case t of
    ElemTy S -> Just $ ElemTy U -- S -> U
    _ | isElemOrSet R t -> mkSetType U  -- R -> 2^U
    _ -> Nothing
  Roles _ | any (`isElemOrSet` t) [U, P, S]
    -> mkSetType R -- U + P + S -> 2^R
  Sessions | isElemOrSet U t -> mkSetType S  -- U -> 2^S
  Permissions _ | isElemOrSet R t -> mkSetType P -- R -> 2^P
  Objects | isElemOrSet P t -> mkSetType OBJ  -- P -> 2^OBJ
  _ -> Nothing

isElemOrSet :: Base -> SetType -> Bool
isElemOrSet s = (== Just s) . mElemOrSet

mElemOrSet :: SetType -> Maybe Base
mElemOrSet t = case t of
  ElemTy i -> Just i
  Set (ElemTy i) -> Just i -- p 215 "general notation device"
  _ -> Nothing

mkSetType :: Base -> Maybe SetType
mkSetType = Just . setTy

setTy :: Base -> SetType
setTy = Set . ElemTy

isSet :: SetType -> Bool
isSet = isJust . elemType

elemType :: SetType -> Maybe SetType
elemType t = case t of
  Set s -> Just s
  _ -> Nothing

isElem :: SetType -> Bool
isElem t = case t of
  ElemTy _ -> True
  _ -> False
