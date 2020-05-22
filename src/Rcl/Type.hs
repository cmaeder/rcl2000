module Rcl.Type (addPrimTypes, elemType, isElem, mBaseType, typeErrors,
                 typeOfSet, wellTyped) where

import Control.Monad (when)
import Control.Monad.State (State, evalState, execState, modify)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Print (ppSet, ppStmt)

addPrimTypes :: UserTypes -> UserTypes
addPrimTypes = flip
  (foldr $ \ b -> Map.insertWith Set.union (show b) $ mkSetType b) primTypes

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

ty :: UserTypes -> Stmt -> State String ()
ty us s = case s of
  BoolOp _ s1 s2 -> ty us s1 >> ty us s2
  CmpOp o e1 e2 -> do
    t1 <- tyTerm us e1
    t2 <- tyTerm us e2
    let md t = report $ t ++ ": " ++ ppStmt s
        ds es = do
          let l = Set.size es
          when (l == 0) $ md "wrongly typed"
          when (l > 1) $ md "ambiguous statement"
    case (t1, t2) of
        (SetTy s1, SetTy s2) -> do
            let ts = if o `elem` [Eq, Ne] then compatSetTys s1 s2 else
                  if o == Elem then Set.filter (\ t -> case t of
                  SetOf e -> Set.member e s1
                  _ -> False) s2 else Set.empty
            ds ts
        (SetTy s1, EmptySetTy) -> ds s1
        (NatTy, NatTy) | o /= Elem -> pure ()
        _ -> md "wrong comparison"
    pure () -- error reported earlier

tyTerm :: UserTypes -> Term -> State String Type
tyTerm us t = case t of
  Term b s -> do
    r <- tySet us s
    let m = getType r
    case b of
      Card -> do
        when (Set.size m > 1) . report $ "ambiguous set: " ++ ppSet s
        pure NatTy
      TheSet -> pure $ SetTy m
  EmptySet -> pure EmptySetTy
  Num _ -> pure NatTy

disambig :: SetType -> Set -> State String Set
disambig t s = case s of
  BinOp o (UnOp (Typed ts1) s1) (UnOp (Typed ts2) s2) -> case o of
    Operations _ -> do
      let Right t1 = getUniqueType s1 ts1
          Right t2 = getUniqueType s2 ts2
      m1 <- disambig t1 s1
      m2 <- disambig t2 s2
      pure . BinOp o (mkTypedSet ts1 m1) $ mkTypedSet ts2 m2
    _ -> do
      let r1 = Set.filter (eqOrElem t) ts1
          r2 = Set.filter (eqOrElem t) ts2
      pure s

eqOrElem :: SetType -> SetType -> Bool
eqOrElem t1 t2 = case t1 of
  SetOf s -> s == t2
  _ -> t1 == t2

getUniqueType :: Set -> Set.Set SetType -> Either String SetType
getUniqueType s ts = case Set.toList ts of
  [t] -> Right t
  [] -> Left $ "missing type for: " ++ ppSet s
  _ -> Left $ "ambiguous type for '" ++ ppSet s ++ "': " ++ ppType ts

tySet :: UserTypes -> Set -> State String Set
tySet us = let md t s = report $ t ++ ": " ++ ppSet s in foldSet FoldSet
  { foldBin = \ _ o s1 s2 -> do
      a1 <- s1
      a2 <- s2
      let m1 = getType a1
          m2 = getType a2
          s = BinOp o a1 a2
      case o of
        Operations _ -> do
          let r1 = Set.filter (\ t1 -> any (`isElemOrSet` t1) [R, U]) m1
              l1 = Set.size r1
              r2 = Set.filter (isElemOrSet OBJ) m2
              l2 = Set.size r2
          when (l1 == 0) $ md "expected role or user as first argument" s
          when (l2 == 0) $ md "expected object as second argument" s
          when (l1 > 1) $ md "ambiguous first argument" s
          when (l2 > 1) $ md "ambiguous second argument" s
          pure . mkTypedSet (mkSetType OP)
            . BinOp o (mkTypedSet r1 $ getUntypedSet a1) . mkTypedSet r2
            $ getUntypedSet a2 -- R x OBJ -> 2^OP
        _ -> do
          let ts = compatSetTys m1 m2
          when (Set.null ts) $ md "wrongly typed set operation" s
          pure $ mkTypedSet ts s
  , foldUn = \ _ o s1 -> do
      a1 <- s1
      let t1 = getType a1
          s = UnOp o a1
      if Set.null t1 then pure $ mkTypedSet t1 s -- was reported earlier
        else do
        let rs = tyAppl o t1
            ts = Set.unions rs
            l = Set.size ts
        when (l == 0) $ md "wrongly typed application" s
        when (l > 0 && l < length rs) $ md "ambiguous application" s
        pure $ mkTypedSet ts s
  , foldPrim = \ s -> case s of
      PrimSet p -> do
        let ts = Map.findWithDefault Set.empty p us
        when (Set.null ts) $ md "unknown base set" s
        pure $ mkTypedSet ts s
      _ -> pure s
}

mkTypedSet :: Set.Set SetType -> Set -> Set
mkTypedSet = UnOp . Typed

getType :: Set -> Set.Set SetType
getType s = case s of
  UnOp (Typed ts) _ -> ts
  Var (MkVar _ _ ts) -> ts
  _ -> Set.empty

getUntypedSet :: Set -> Set
getUntypedSet s = case s of
  UnOp (Typed _) e -> e
  _ -> s

compatSetTys :: Set.Set SetType -> Set.Set SetType -> Set.Set SetType
compatSetTys s1 s2 =
  Set.unions [compatSetTysAux t1 t2 | t1 <- Set.toList s1, t2 <- Set.toList s2]

compatSetTysAux :: SetType -> SetType -> Set.Set SetType
compatSetTysAux t1 t2 = case (t1, t2) of
  (SetOf s1, _) | s1 == t2 -> Set.singleton t1
  (_, SetOf s2) | t1 == s2 -> Set.singleton t2
  _ -> if t1 == t2 then Set.singleton t1 else Set.empty

tyAppl :: UnOp -> Set.Set SetType -> [Set.Set SetType]
tyAppl o = map (tyApplAux o) . Set.toList

tyApplAux :: UnOp -> SetType -> Set.Set SetType
tyApplAux o t = case o of
  OE -> case t of
    SetOf s -> Set.singleton s
    ElemTy _ -> Set.empty
  AO -> case t of
    SetOf _ -> Set.singleton t
    ElemTy _ -> Set.empty
  User _ b -> case t of
    ElemTy S | b /= Star -> Set.singleton $ ElemTy U -- S -> U
    _ | isElemOrSet R t -> mkSetType U  -- R -> 2^U
    _ -> Set.empty
  Roles _ | any (`isElemOrSet` t) [U, P, S]
    -> mkSetType R -- U + P + S -> 2^R
  Sessions | isElemOrSet U t -> mkSetType S  -- U -> 2^S
  Permissions _ | any (`isElemOrSet` t) [R, U, S] -> mkSetType P -- R -> 2^P
  Object _ | isElemOrSet P t -> mkSetType OBJ  -- P -> 2^OBJ
  Iors _ _ | isElemOrSet R t -> mkSetType R  -- extra functions R -> 2^R
  _ -> Set.empty

isElemOrSet :: Base -> SetType -> Bool
isElemOrSet s = (== Just s) . mElemOrSet

mElemOrSet :: SetType -> Maybe Base
mElemOrSet t = case t of
  ElemTy i -> Just i
  SetOf (ElemTy i) -> Just i -- p 215 "general notation device"
  _ -> Nothing

mkSetType :: Base -> Set.Set SetType
mkSetType = Set.singleton . SetOf . ElemTy

elemType :: Set.Set SetType -> Set.Set SetType
elemType = Set.foldr (\ t -> case t of
  SetOf s -> Set.insert s
  _ -> id) Set.empty

isElem :: SetType -> Bool
isElem = foldSetType (const False) $ const True
