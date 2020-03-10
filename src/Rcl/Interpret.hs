module Rcl.Interpret (eval, interprets, getUserTypes) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (find, partition)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromMaybe, mapMaybe)
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Data
import Rcl.Print (ppStmt)
import Rcl.Reduce (Vars, runReduce)
import Rcl.Type (wellTyped, typeOfSet)

type Env = IntMap Value

data TermVal = VTerm Value | VEmptySet | VNum Int deriving (Eq, Show)

interprets :: UserTypes -> Model -> [Stmt] -> String
interprets us m l = let
  (ws, es) = partition (isNothing . snd) $ map (\ s -> (s, wellTyped us s)) l
  in unlines $ mapMaybe snd es ++ map (\ (s, _) ->
  case uncurry (interpretError m) $ runReduce us s of
    Nothing -> "verified: " ++ ppStmt s
    Just e -> e) ws

interpretError :: Model -> Stmt -> Vars -> Maybe String
interpretError m s vs = case interpret m IntMap.empty s $ reverse vs of
  Right () -> Nothing
  Left e -> Just $ (if IntMap.null e then "falsified: " else
    "counter example: " ++ printEnv m vs e ++ "\n  in: ") ++ ppStmt s

printEnv :: Model -> Vars -> Env -> String
printEnv m vs e = unwords . map
  (\ (k, v) -> printVar vs k $ stValue m v) $ IntMap.toList e

printVar :: Vars -> Int -> String -> String
printVar vs k e = case find (\ (MkVar i _ _, _) -> i == k) vs of
  Just (v, _) -> stVar v ++ "=" ++ e
  Nothing -> ""

interpret :: Model -> Env -> Stmt -> Vars -> Either Env ()
interpret m e s vs = case vs of
  [] -> if evalStmt m e s then Right () else Left e
  (MkVar i _ _, a) : rs -> case eval m e a of
    Ints is -> mapM_
      (\ j -> interpret m (IntMap.insert i (Ints $ IntSet.singleton j) e) s rs)
      $ IntSet.toList is
    VSet es -> mapM_
      (\ j -> interpret m (IntMap.insert i j e) s rs)
      $ Set.toList es

evalStmt :: Model -> Env -> Stmt -> Bool
evalStmt m e = foldStmt FoldStmt
  { foldBool = \ _ o b1 b2 -> (if o == Impl then (<=) else (&&)) b1 b2
  , foldCmp = \ _ o t1 t2 -> case o of
      Elem -> case (t1, t2) of
        (VTerm (Ints is), VTerm (Ints js)) ->
           is `IntSet.isSubsetOf` js && IntSet.size is == 1
        (VTerm v1, VTerm (VSet s)) -> Set.member v1 s
        _ -> error "evalStmt: elem"
      Eq -> case (t1, t2) of
        (VTerm v1, VEmptySet) -> vNull v1
        _ -> t1 == t2
      Ne -> case (t1, t2) of
        (VTerm v1, VEmptySet) -> not $ vNull v1
        _ -> t1 /= t2
      _ -> case (t1, t2) of
        (VNum i1, VNum i2) -> cmpOp o i1 i2
        _ -> error "evalStmt: nums" } (evalTerm m e)

cmpOp :: CmpOp -> Int -> Int -> Bool
cmpOp o = case o of
  Le -> (<=)
  Lt -> (<)
  Ge -> (>=)
  Gt -> (>)
  Eq -> (==)
  Ne -> (/=)
  Elem -> error "cmpOp"

evalTerm :: Model -> Env -> Term -> TermVal
evalTerm m e t = case t of
  Term card s -> let v = eval m e s in
    if card then VNum $ vSize v else VTerm v
  EmptySet -> VEmptySet
  Num n -> VNum n

eval :: Model -> Env -> Set -> Value
eval m e = let us = getUserTypes m in foldSet FoldSet
  { foldBin = \ _ o v1 v2 -> case o of
      Ops -> Ints . IntSet.unions . map
        (\ p -> Map.findWithDefault IntSet.empty p $ opsMap m)
        $ case (v1, v2) of
          (Ints rs, Ints os) ->
            [ (r, ob) | r <- IntSet.toList rs, ob <- IntSet.toList os ]
          _ -> []
      Minus -> case (v1, v2) of
        (VSet s, _) -> assert "eval1" (Set.member v2 s) . VSet $ Set.delete v2 s
        (Ints is, Ints js) -> assert "eval2"
          (js `IntSet.isSubsetOf` is && IntSet.size js == 1)
          . Ints $ is IntSet.\\ js
        _ -> Ints IntSet.empty
      _ -> assert "eval3" (sameNesting v1 v2) $ case (v1, v2) of
        (Ints is, Ints js) -> Ints
          $ (if o == Inter then IntSet.intersection else IntSet.union) is js
        _ -> VSet . (if o == Inter then Set.intersection else Set.union)
          (toVSet v1) $ toVSet v2
  , foldUn = \ (UnOp _ s) o (Ints is) -> let p = sUnOp (typeOfSet us s) o
      in case o of
      Permissions True -> Ints . apply m p $ apply m "r" is
      Roles True -> Ints . apply m "r" $ apply m p is
      _ -> Ints $ apply m p is
  , foldPrim = evalPrim m e }

evalPrim :: Model -> Env -> Set -> Value
evalPrim m e s = case s of
  PrimSet p -> maybe (error $ "evalPrim1: " ++ p) (\ (_, v, _) -> v)
    . Map.lookup p $ userSets m
  Var v@(MkVar i _ _) -> fromMaybe (error $ "evalPrim2: " ++ stVar v)
    $ IntMap.lookup i e
  _ -> error "evalPrim: not primitive"

toVSet :: Value -> Set.Set Value
toVSet v = case v of
  VSet s -> s
  Ints is -> if IntSet.null is then Set.empty else Set.singleton v

vSize :: Value -> Int
vSize v = case v of
  VSet s -> Set.size s
  Ints is -> IntSet.size is

vNull :: Value -> Bool
vNull v = case v of
  VSet s -> Set.null s
  Ints is -> IntSet.null is

sameNesting :: Value -> Value -> Bool
sameNesting v1 v2 = case (v1, v2) of
  (Ints _, Ints _) -> True
  (VSet s1, VSet s2) -> case (Set.minView s1, Set.minView s2) of
    (Just (e1, _), Just (e2, _)) -> sameNesting e1 e2
    _ -> True -- one set is empty and nothing to check
  (Ints _, VSet _) -> sameNesting (VSet $ toVSet v1) v2
  (VSet _, Ints _) -> sameNesting v1 . VSet $ toVSet v2

apply :: Model -> String -> IntSet -> IntSet
apply m s is = let
  im = Map.findWithDefault (error $ "apply: " ++ s) s $ fctMap m
  in IntSet.unions . map (\ i -> IntMap.findWithDefault IntSet.empty i im)
  $ IntSet.toList is
