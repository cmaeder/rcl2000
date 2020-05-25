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
ty us s = case s of
  BoolOp o s1 s2 -> do
    r1 <- ty us s1
    r2 <- ty us s2
    pure $ BoolOp o r1 r2
  CmpOp o e1 e2 -> do
    t1 <- tyTerm us e1
    t2 <- tyTerm us e2
    let md t = report $ t ++ ": " ++ ppStmt s
        ds es = do
          let l = Set.size es
          when (l == 0) $ md "wrongly typed"
          when (l > 1) $ md "ambiguous statement"
        r = CmpOp o t1 t2
    case (t1, t2) of
        (Term TheSet (UnOp (Typed ts1) s1), Term TheSet (UnOp (Typed ts2) s2))
          -> do
            let ts = case o of
                  Elem -> Set.filter (ifSet (`Set.member` ts1) False) ts2
                  _ | o `elem` [Eq, Ne] -> compatSetTys ts1 ts2
                  _ -> Set.empty
            if Set.size ts == 1 then do
                let t = getUniqueType ts
                    rs1 = Set.filter (if o == Elem then (== t) . SetOf
                      else eqOrElem t) ts1
                    rs2 = Set.filter
                      (if o == Elem then (== t) else eqOrElem t) ts2
                if Set.size rs1 == 1 && Set.size rs2 == 1 then do
                    r1 <- disambig (getUniqueType rs1) s1
                    r2 <- disambig (getUniqueType rs2) s2
                    pure . CmpOp o (Term TheSet r1) $ Term TheSet r2
                  else do
                    ds rs1
                    ds rs2
                    pure r
              else do
                ds ts
                pure r
        (Term TheSet (UnOp (Typed ts1) s1), EmptySet) | o `elem` [Eq, Ne] ->
          if Set.size ts1 == 1 then do
              r1 <- disambig (getUniqueType ts1) s1
              pure $ CmpOp o (Term TheSet r1) e2
            else do
              ds ts1
              pure r
        (Term Card _, Term Card _) | o /= Elem -> pure r
        (Term Card _, Num _) | o /= Elem -> pure r
        (Num _, Term Card _) | o /= Elem -> pure r
        _ -> do
          md "wrong comparison"
          pure r

tyTerm :: UserTypes -> Term -> State String Term
tyTerm us t = case t of
  Term b s -> do
    r <- tySet us s
    let m = getType r
    case b of
      Card ->
        if Set.size m == 1 then do
            n <- disambig (getUniqueType m) r
            pure $ Term b n
          else do
            report $ "ambiguous set: " ++ ppSet s
            pure t
      TheSet -> pure $ Term b r
  _ -> pure t

disambig :: SetType -> Set -> State String Set
disambig t s = let md m = report $ m ++ ": " ++ ppSet s in case s of
  BinOp o (UnOp (Typed ts1) s1) (UnOp (Typed ts2) s2) -> do
    let (r1, r2) = case o of
          Operations _ -> (ts1, ts2)
          _ -> (Set.filter (eqOrElem t) ts1, Set.filter (eqOrElem t) ts2)
    if Set.size r1 == 1 && Set.size r2 == 1 then do
          let t1 = getUniqueType r1
              t2 = getUniqueType r2
          m1 <- disambig t1 s1
          m2 <- disambig t2 s2
          pure . BinOp o (mkTypedSet r1 m1) $ mkTypedSet r2 m2
        else do
          md "ambiguous"
          pure s
  UnOp o (UnOp (Typed ts1) s1) -> do
    let r1 = Set.filter (getArgumentTypes o t) ts1
    if Set.size r1 == 1 then do
        let t1 = getUniqueType r1
        m1 <- disambig t1 s1
        pure . UnOp o $ mkTypedSet r1 m1
      else do
        md "ambiguous"
        pure s
  _ -> pure s

getArgumentTypes :: UnOp -> SetType -> SetType -> Bool
getArgumentTypes o t a = case o of
  OE -> a == SetOf t
  AO -> a == t
  _ -> True

eqOrElem :: SetType -> SetType -> Bool
eqOrElem t1 t2 = case t1 of
  SetOf s -> s == t2
  _ -> t1 == t2

getUniqueType :: Set.Set SetType -> SetType
getUniqueType ts = case Set.toList ts of
  [t] -> t
  _ -> error "getUniqueType"

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
   { foldBin = \ _ o s1 s2 -> do
      a1 <- s1
      a2 <- s2
      case o of
        Operations _ -> do
          b1 <- filterType True (\ t1 -> any (`isElemOrSet` t1) [R, U]) a1
          b2 <- filterType True (isElemOrSet OBJ) a2
          pure . mkTypedSet (Set.singleton $ toSet OP)
            $ BinOp o b1 b2 -- R + U x OBJ -> 2^OP
        _ -> do
          let m1 = getType a1
              m2 = getType a2
              s = BinOp o a1 a2
              ts = compatSetTys m1 m2
          when (Set.null ts) $ md "wrongly typed set operation" s
          pure $ mkTypedSet ts s
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
compatSetTys s1 s2 =
  Set.unions [compatSetTysAux t1 t2 | t1 <- Set.toList s1, t2 <- Set.toList s2]

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
