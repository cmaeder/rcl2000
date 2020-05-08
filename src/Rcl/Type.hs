module Rcl.Type (typeErrors, wellTyped, typeOfSet, elemType, isElem) where

import Control.Monad (unless, when)
import Control.Monad.State (State, evalState, execState, modify)
import Data.List (find)
import qualified Data.Map as Map (lookup)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Rcl.Ast
import Rcl.Print (ppSet, ppStmt)

typeErrors :: UserTypes -> [Stmt] -> String
typeErrors us = unlines . mapMaybe (wellTyped us)

typeOfSet :: UserTypes -> Set -> Maybe SetType
typeOfSet us s = evalState (tySet us s) []

wellTyped :: UserTypes -> Stmt -> Maybe String
wellTyped us s = case execState (ty us s) [] of
  "" -> Nothing
  e -> Just $ e ++ "\n  in: " ++ ppStmt s

report :: String -> State String ()
report = modify . const

ty :: UserTypes -> Stmt -> State String ()
ty us s = case s of
  BoolOp _ s1 s2 -> ty us s1 >> ty us s2
  CmpOp o e1 e2 -> do
    m1 <- tyTerm us e1
    m2 <- tyTerm us e2
    let md = report $ "wrongly typed relation: " ++ ppStmt s
    case (m1, m2) of
      (Just t1, Just t2) -> case (t1, t2) of
        (SetTy s1, SetTy s2) ->
          unless (elemType s2 == Just s1 && o == Elem
            || o `elem` [Eq, Ne] && isJust (compatSetTys s1 s2)) md
        (SetTy _, EmptySetTy) -> pure ()
        (NatTy, NatTy) | o /= Elem -> pure ()
        _ -> md
      _ -> pure () -- error reported earlier

tyTerm :: UserTypes -> Term -> State String (Maybe Type)
tyTerm us t = case t of
  Term b s -> do
    m <- tySet us s
    pure $ case b of
      Card -> Just NatTy
      TheSet -> fmap SetTy m
  EmptySet -> pure $ Just EmptySetTy
  Num _ -> pure $ Just NatTy

tySet :: UserTypes -> Set -> State String (Maybe SetType)
tySet us s = let md t = report $ t ++ ": " ++ ppSet s in case s of
  BinOp o s1 s2 -> do
    m1 <- tySet us s1
    m2 <- tySet us s2
    case (m1, m2) of
      (Just t1, Just t2) -> case o of
        Operations _ -> do
          unless (isElemOrSet R t1 && isElemOrSet OBJ t2)
            $ md "expected role and object arguments"
          pure $ mkSetType OP -- R x OBJ -> 2^OP
        Minus -> do
          unless (elemType t1 == Just t2)
            $ md "wrongly typed set minus element"
          pure $ Just t1
        _ -> do
          let t = compatSetTys t1 t2
          when (isNothing t) $ md "wrongly typed set operation"
          pure t
      _ -> pure Nothing
  UnOp o s1 -> do
    t1 <- tySet us s1
    case t1 of
      Nothing -> pure t1 -- was reported earlier
      Just st -> do
        let t = tyAppl o st
        when (isNothing t) $ md "wrongly typed application"
        pure t
  Var (MkVar _ _ t) -> pure t
  PrimSet p -> case find ((== p) . show) primTypes of
    Just b -> pure $ mkSetType b
    Nothing -> case Map.lookup p us of
      Just t -> pure $ Just t
      Nothing -> do
        md "unknown base set"
        pure Nothing

compatSetTys :: SetType -> SetType -> Maybe SetType
compatSetTys t1 t2 = case (mElemOrSet t1, mElemOrSet t2) of
    (Just s1, Just s2) | s1 == s2 -> mkSetType s1 -- treat elements as sets
    _ -> Nothing

tyAppl :: UnOp -> SetType -> Maybe SetType
tyAppl o t = case o of
  OE -> elemType t
  AO | isSet t -> Just t
  User _ b -> case t of
    ElemTy S -> if b == Star then Nothing else Just $ ElemTy U -- S -> U
    _ | isElemOrSet R t -> mkSetType U  -- R -> 2^U
    _ -> Nothing
  Roles _ | any (`isElemOrSet` t) [U, P, S]
    -> mkSetType R -- U + P + S -> 2^R
  Sessions | isElemOrSet U t -> mkSetType S  -- U -> 2^S
  Permissions _ | isElemOrSet R t -> mkSetType P -- R -> 2^P
  Object _ | isElemOrSet P t -> mkSetType OBJ  -- P -> 2^OBJ
  Iors _ _ | isElemOrSet R t -> mkSetType R  -- extra functions R -> 2^R
  _ -> Nothing

isElemOrSet :: Base -> SetType -> Bool
isElemOrSet s = (== Just s) . mElemOrSet

mElemOrSet :: SetType -> Maybe Base
mElemOrSet t = case t of
  ElemTy i -> Just i
  SetOf (ElemTy i) -> Just i -- p 215 "general notation device"
  _ -> Nothing

mkSetType :: Base -> Maybe SetType
mkSetType = Just . SetOf . ElemTy

isSet :: SetType -> Bool
isSet = isJust . elemType

elemType :: SetType -> Maybe SetType
elemType t = case t of
  SetOf s -> Just s
  _ -> Nothing

isElem :: SetType -> Bool
isElem = foldSetType (const False) $ const True
