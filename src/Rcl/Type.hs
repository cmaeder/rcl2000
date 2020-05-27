module Rcl.Type (addPrimTypes, elemType, isElem, mBaseType, typeErrors,
                 typeOfSet, wellTyped) where

import Control.Monad (unless, when)
import Control.Monad.State (State, evalState, execState, modify)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Print (ppSet, ppStmt)

addPrimTypes :: UserTypes -> UserTypes
addPrimTypes = flip (foldr $ \ b -> Map.insertWith Set.union (show b)
  . Set.singleton $ toSet b) primTypes

typeErrors :: UserTypes -> [Stmt] -> String
typeErrors us = unlines . mapMaybe (wellTyped us)

mBaseType :: UserTypes -> Set -> Set.Set Base
mBaseType us = Set.map baseType . typeOfSet us

typeOfSet :: UserTypes -> Set -> Set.Set SetType
typeOfSet us s = getType $ evalState (tySet us s) []

wellTyped :: UserTypes -> Stmt -> Maybe String
wellTyped us s = case execState (ty us s) [] of
  "" -> Nothing
  e -> Just $ e ++ "\n  in: " ++ ppStmt s

report :: String -> State String ()
report = modify . const

ty :: UserTypes -> Stmt -> State String Stmt
ty us = foldStmt FoldStmt
  { foldBool = \ _ o s1 s2 -> do
    r1 <- s1
    BoolOp o r1 <$> s2
  , foldCmp = \ _ o e1 e2 -> do
    t1 <- e1
    t2 <- e2
    let r = CmpOp o t1 t2
        md m = report $ m ++ ": " ++ ppStmt r
    case (t1, t2) of
        (Term TheSet s1, Term TheSet s2) | o `elem` [Eq, Ne, Elem] -> do
            let ts1 = getType s1
                ts2 = getType s2
                ts = if o == Elem then
                  Set.filter (ifSet (`Set.member` ts1) False) ts2
                  else compatSetTys ts1 ts2
                filt t = any (`Set.member` ts) [t, SetOf t]
                filt1 t = if o == Elem then SetOf t `Set.member` ts else filt t
                filt2 t = if o == Elem then t `Set.member` ts else filt t
            b1 <- filterType True filt1 s1
            b2 <- filterType True filt2 s2
            when (null ts) $ md "wrongly typed relation"
            pure . CmpOp o (Term TheSet b1) $ Term TheSet b2
        (Term TheSet s1, EmptySet) | o `elem` [Eq, Ne] -> do
          n <- filterType True (const True) s1
          pure $ CmpOp o (Term TheSet n) t2
        (Term Card _, Term Card _) | o /= Elem -> pure r
        (Term Card _, Num _) | o /= Elem -> pure r
        (Num _, Term Card _) | o /= Elem -> pure r
        _ -> do
          md "wrong comparison"
          pure r } (tyTerm us)

tyTerm :: UserTypes -> Term -> State String Term
tyTerm us t = case t of
  Term b s -> do
    r <- tySet us s
    case b of
      Card -> do
        n <- filterType True (const True) s
        pure $ Term b n
      TheSet -> pure $ Term b r
  _ -> pure t

disambig :: SetType -> Set -> State String Set
disambig t s = let
  rt = mkTypedSet (Set.singleton t)
  r = rt s in case s of
  BinOp o s1 s2 -> case o of
    Operations _ -> pure r
    _ -> do
      m1 <- filterType True (eqOrElem t) s1
      m2 <- filterType True (eqOrElem t) s2
      pure . rt $ BinOp o m1 m2
  UnOp o s1 -> if isOp o then pure r else do
      r1 <- filterType True (\ t1 -> case o of
        OE -> SetOf t == t1
        AO -> t1 == t
        Typed ts -> t1 == t && Set.member t ts
        _ -> False) s1
      pure . rt $ UnOp o r1
  _ -> pure r

eqOrElem :: SetType -> SetType -> Bool
eqOrElem t1 t2 = case t1 of
  SetOf s -> s == t2
  _ -> t1 == t2

filterType :: Bool -> (SetType -> Bool) -> Set -> State String Set
filterType b f s = let
  md m = report $ m ++ ": " ++ ppSet s
  t = getType s
  l = Set.toList t
  r = filter f l
  in case r of
  [] -> do
    unless (null l) $ md "wrongly typed set"
    pure s
  [a] -> case l of
    _ : _ : _ -> disambig a $ getUntypedSet s
    _ -> pure s
  _ -> do
    let rs = Set.fromList r
    when b . md $ "ambiguous set (" ++ ppType rs ++ ")"
    pure . mkTypedSet rs $ getUntypedSet s

tySet :: UserTypes -> Set -> State String Set
tySet us = let
  md t s = report $ t ++ ": " ++ ppSet s
  in foldSet FoldSet
   { foldBin = \ s o s1 s2 -> do
      a1 <- s1
      a2 <- s2
      case o of
        Operations _ -> do
          b1 <- filterType True (\ t -> any (`isElemOrSet` t) [R, U]) a1
          b2 <- filterType True (isElemOrSet OBJ) a2
          pure . mkTypedSet (Set.singleton $ toSet OP)
            $ BinOp o b1 b2 -- R + U x OBJ -> 2^OP
        _ -> do
          let ts = compatSetTys (getType a1) $ getType a2
              filt t = any (`Set.member` ts) [t, SetOf t]
          b1 <- filterType False filt a1
          b2 <- filterType False filt a2
          when (null ts) $ md "wrongly typed set operation" s
          pure . mkTypedSet ts $ BinOp o b1 b2
  , foldUn = \ _ o s1 -> do
      a1 <- s1
      b1 <- filterType (isOp o) (opArg o) a1
      pure $ opResult o b1
  , foldPrim = \ s -> case s of
      PrimSet p -> do
        let ts = Map.findWithDefault Set.empty p us
        when (Set.null ts) $ md "unknown base set" s
        pure $ mkTypedSet ts s
      _ -> pure s
}

isOp :: UnOp -> Bool
isOp o = case o of
  OE -> False
  AO -> False
  Typed _ -> False
  _ -> True

mkTypedSet :: Set.Set SetType -> Set -> Set
mkTypedSet ts = if Set.null ts then id else UnOp $ Typed ts

getType :: Set -> Set.Set SetType
getType s = case s of
  UnOp (Typed ts) _ -> ts
  Var (MkVar _ _ ts) -> ts
  _ -> Set.empty

getUntypedSet :: Set -> Set
getUntypedSet s = case s of
  UnOp (Typed _) e -> getUntypedSet e
  _ -> s

compatSetTys :: Set.Set SetType -> Set.Set SetType -> Set.Set SetType
compatSetTys s1 s2 = case Set.toList s1 of
  [] -> s2
  l1 -> case Set.toList s2 of
    [] -> s1
    l2 -> Set.unions [compatSetTysAux t1 t2 | t1 <- l1, t2 <- l2]

compatSetTysAux :: SetType -> SetType -> Set.Set SetType
compatSetTysAux t1 t2 = case (t1, t2) of
  (SetOf s1, _) | s1 == t2 -> Set.singleton t1
  (_, SetOf s2) | t1 == s2 -> Set.singleton t2
  _ -> if t1 == t2 then Set.singleton t1 else Set.empty

opArg :: UnOp -> SetType -> Bool
opArg o t = case o of
  User _ b -> b /= Star && t == ElemTy S || isElemOrSet R t
  Roles _ -> any (`isElemOrSet` t) [U, P, S]
  Sessions -> isElemOrSet U t
  Permissions _ -> any (`isElemOrSet` t) [R, U, S]
  Object _ -> isElemOrSet P t
  Iors _ _ -> isElemOrSet R t
  Typed ts -> Set.member t ts
  _ -> not $ isElem t

opResult :: UnOp -> Set -> Set
opResult o s = let
  ts = getType s
  mkSetType = Set.singleton . toSet
  in mkTypedSet (case o of
  OE -> elemType ts
  AO -> ts
  Typed _ -> ts
  User _ _ -> if ElemTy S `Set.member` ts then Set.singleton $ ElemTy U
    else mkSetType U
  Roles _ -> mkSetType R
  Sessions -> mkSetType S
  Permissions _ -> mkSetType P
  Object _ -> mkSetType OBJ
  Iors _ _ -> mkSetType R) $ UnOp o s

isElemOrSet :: Base -> SetType -> Bool
isElemOrSet s = (== Just s) . mElemOrSet

mElemOrSet :: SetType -> Maybe Base
mElemOrSet t = case t of
  ElemTy i -> Just i
  SetOf (ElemTy i) -> Just i -- p 215 "general notation device"
  _ -> Nothing

toSet :: Base -> SetType
toSet = SetOf . ElemTy

elemType :: Set.Set SetType -> Set.Set SetType
elemType = Set.foldr (ifSet Set.insert id) Set.empty

ifSet :: (SetType -> a) -> a -> SetType -> a
ifSet f c s = case s of
  SetOf e -> f e
  _ -> c

isElem :: SetType -> Bool
isElem = ifSet (const False) True
