module Rcl.Type (addPrimTypes, elemType, isElem, mBaseType, typeErrors,
                 typeSet, wellTyped) where

import Control.Monad (unless, when)
import Control.Monad.State (State, modify, runState)
import Data.Either (lefts)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Print (ppSet, ppStmt)

addPrimTypes :: UserTypes -> UserTypes
addPrimTypes = flip (foldr $ \ b -> Map.insertWith Set.union (show b)
  . Set.singleton $ toSet b) primTypes

typeErrors :: UserTypes -> [Stmt] -> String
typeErrors us = unlines . lefts . map (wellTyped us)

mBaseType :: Set -> Set.Set Base
mBaseType = Set.map baseType . getType

typeSet :: UserTypes -> Set -> Either String Set
typeSet us s = case runState (tySet us s >>=
    filterType ("set: " ++ ppSet s) True (const True)) [] of
  (t, []) -> Right t
  (_, l) -> Left $ unlines l

wellTyped :: UserTypes -> Stmt -> Either String Stmt
wellTyped us s = case runState (tyStmt us s) [] of
  (t, []) -> Right t
  (_, e) -> Left $ unlines e ++ "  in: " ++ ppStmt s

report :: String -> State [String] ()
report t = modify (t :)

tyStmt :: UserTypes -> Stmt -> State [String] Stmt
tyStmt = foldStmt FoldStmt
  { foldBool = \ _ o s1 s2 -> do
    r1 <- s1
    BoolOp o r1 <$> s2
  , foldCmp = \ _ o e1 e2 -> do
    t1 <- e1
    t2 <- e2
    let r = CmpOp o t1 t2
        str = ppStmt r
        md m = report $ m ++ ": " ++ str
        ft = filterType ("in relation: " ++ str) True
    case (t1, t2) of
        (Term TheSet s1, Term TheSet s2) | o `elem` [Eq, Ne, Elem] -> do
            let ts1 = getType s1
                ts2 = getType s2
                ts = if o == Elem then
                  Set.filter (ifSet (`Set.member` ts1) False) ts2
                  else compatSetTys ts1 ts2
                filt t = any (`Set.member` ts) [t, SetOf t]
                filt0 f = if Set.null ts then const True else f
                filt1 t = if o == Elem then SetOf t `Set.member` ts else filt t
                filt2 t = if o == Elem then t `Set.member` ts else filt t
            b1 <- ft (filt0 filt1) s1
            b2 <- ft (filt0 filt2) s2
            when (Set.null ts) $ md "wrongly typed relation"
            pure . CmpOp o (Term TheSet b1) $ Term TheSet b2
        (Term TheSet s1, EmptySet) | o `elem` [Eq, Ne] -> do
          n <- ft (const True) s1
          pure $ CmpOp o (Term TheSet n) t2
        (Term Card _, Term Card _) | o /= Elem -> pure r
        (Term Card _, Num _) | o /= Elem -> pure r
        (Num _, Term Card _) | o /= Elem -> pure r
        _ -> do
          md "wrong comparison"
          pure r } . tyTerm

tyTerm :: UserTypes -> Term -> State [String] Term
tyTerm us t = case t of
  Term b s -> do
    r <- tySet us s
    case b of
      Card -> do
        n <- filterType ("in cardinality of: " ++ ppSet s) True (const True) r
        pure $ Term b n
      TheSet -> pure $ Term b r
  _ -> pure t

disambig :: String -> SetType -> Set -> State [String] Set
disambig str t s = let
  rt = mkTypedSet (Set.singleton t)
  r = rt s
  filt = filterType str True
  ft = filt (\ ts -> t `elem` [ts, SetOf ts])
  in case s of
  BinOp o s1 s2 -> case o of
    Operations _ -> pure r
    _ -> do
      m1 <- ft s1
      m2 <- ft s2
      pure . rt $ BinOp o m1 m2
  UnOp o s1 -> if isOp o then pure r else do
      r1 <- filt (\ t1 -> case o of
        OE -> SetOf t == t1
        AO -> t1 == t
        Typed ts -> t1 == t && Set.member t ts
        _ -> False) s1
      pure . rt $ UnOp o r1
  _ -> pure r

filterType :: String -> Bool -> (SetType -> Bool) -> Set -> State [String] Set
filterType str b f s = let
  md m = report $ m ++ " " ++ str
  t = getType s
  l = Set.toList t
  r = filter f l
  rs = Set.fromList r
  mt = mkTypedSet rs
  in case r of
  [] -> do
    unless (null l) $ md "wrongly typed"
    pure $ getUntypedSet s
  [a] -> case l of
    _ : _ : _ -> disambig str a $ getUntypedSet s
    _ -> pure s
  _ -> do
    when b . md $ "ambiguous '" ++ ppSet s ++ ":" ++ ppType rs ++ "'"
    pure . mt $ getUntypedSet s

tySet :: UserTypes -> Set -> State [String] Set
tySet us = let
  md t s = report $ t ++ ": " ++ ppSet s
  in foldSet FoldSet
   { foldBin = \ s o s1 s2 -> do
      a1 <- s1
      a2 <- s2
      case o of
        Operations _ -> do
          b1 <- filterType ("1st arg: " ++ ppSet s)
            True (\ t -> any (`isElemOrSet` t) [R, U]) a1
          b2 <- filterType ("2nd arg: " ++ ppSet s) True (isElemOrSet OBJ) a2
          pure . mkTypedSet (Set.singleton $ toSet OP)
            $ BinOp o b1 b2 -- R + U x OBJ -> 2^OP
        _ -> do
          let ts = Set.map (\ t -> if isElem t then SetOf t else t)
                . compatSetTys (getType a1) $ getType a2
              filt t = any (`Set.member` ts) [t, SetOf t]
              ft = filterType ("in set: " ++ ppSet s) (Set.size ts <= 1)
                (if Set.null ts then const True else filt)
          b1 <- ft a1
          b2 <- ft a2
          when (Set.null ts) $ md "wrongly typed set operation" s
          pure . mkTypedSet ts $ BinOp o b1 b2
  , foldUn = \ s o s1 -> do
      a1 <- s1
      b1 <- filterType ("application: " ++ ppSet s) (isOp o) (opArg o) a1
      pure $ opResult o b1
  , foldPrim = \ s -> case s of
      PrimSet p -> do
        let ts = Map.findWithDefault Set.empty p us
        when (Set.null ts) $ md "unknown base set" s
        pure $ mkTypedSet ts s
      _ -> pure s }

isOp :: UnOp -> Bool
isOp o = case o of
  OE -> False
  AO -> False
  Typed _ -> False
  _ -> True

mkTypedSet :: Set.Set SetType -> Set -> Set
mkTypedSet ts = if Set.null ts then id else UnOp $ Typed ts

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
