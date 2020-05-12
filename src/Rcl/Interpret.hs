module Rcl.Interpret (eval, interprets) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (find, partition)
import qualified Data.Map as Map
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Data
import Rcl.Print (ppSet, ppStmt, prStmt)
import Rcl.Reduce (runReduce)
import Rcl.Type (mBaseType, typeOfSet, wellTyped)

type Env = IntMap Value

data TermVal = VTerm Value | VEmptySet | VNum Int | Error String
  deriving (Eq, Show)

interprets :: UserTypes -> Model -> [Stmt] -> String
interprets us m l = let
  (ws, es) = partition (isNothing . snd) $ map (\ s -> (s, wellTyped us s)) l
  in unlines $ mapMaybe snd es ++ map (\ (s, _) -> let p = runReduce us s in
  (\ r -> prStmt p ++ '\n' : if null r then "verified: " ++ ppStmt s else r)
  $ interpretError us m p) ws

interpretError :: UserTypes -> Model -> (Stmt, Vars) -> String
interpretError us m (s, vs) =
  case interpret us m IntMap.empty (s, reverse vs) of
    Right () -> ""
    Left (Left f) -> f
    Left (Right e) -> (if IntMap.null e then "falsified: " else
      "counter example: " ++ printEnv m vs e ++ "\n  in: ") ++ ppStmt s

printEnv :: Model -> Vars -> Env -> String
printEnv m vs e = unwords . map
  (\ (k, v) -> printVar vs k $ stValue m v) $ IntMap.toList e

printVar :: Vars -> Int -> String -> String
printVar vs k e = case find (\ (MkVar i _ _, _) -> i == k) vs of
  Just (v, _) -> stVar v ++ "=" ++ e
  Nothing -> ""

interpret :: UserTypes -> Model -> Env -> (Stmt, Vars)
  -> Either (Either String Env) ()
interpret us m e (s, vs) = case vs of
  [] -> case evalStmt us m e s of
    Right b -> if b then Right () else Left $ Right e
    Left f -> Left $ Left f
  (MkVar i _ _, a) : rs -> case eval us m e a of
    Right v -> case v of
      Ints is -> mapM_ (\ j -> interpret us m (IntMap.insert i
        (Ints $ IntSet.singleton j) e) (s, rs)) $ IntSet.toList is
      VSet es -> mapM_ (\ j -> interpret us m (IntMap.insert i j e) (s, rs))
        $ Set.toList es
    Left f -> Left $ Left f

evalStmt :: UserTypes -> Model -> Env -> Stmt -> Either String Bool
evalStmt us m e = foldStmt FoldStmt
  { foldBool = \ _ o e1 e2 -> case (e1, e2) of
    (Left _, _) -> e1
    (_, Left _) -> e2
    (Right b1, Right b2) -> Right $ (if o == Impl then (<=) else (&&)) b1 b2
  , foldCmp = \ c o t1 t2 -> let t = ppStmt c in case (t1, t2) of
    (Error s, _) -> Left s
    (_, Error s) -> Left s
    _ -> case o of
      Elem -> case (t1, t2) of
        (VTerm (Ints is), VTerm (Ints js)) ->
           Right $ is `IntSet.isSubsetOf` js && IntSet.size is == 1
        (VTerm v1, VTerm (VSet s)) -> Right $ Set.member v1 s
        _ -> Left $ "unexpected: " ++ t
      Eq -> Right $ case (t1, t2) of
        (VTerm v1, VEmptySet) -> vNull v1
        _ -> t1 == t2
      Ne -> Right $ case (t1, t2) of
        (VTerm v1, VEmptySet) -> not $ vNull v1
        _ -> t1 /= t2
      _ -> case (t1, t2) of
        (VNum i1, VNum i2) -> Right $ cmpOp o i1 i2
        _ -> Left $ "unexpected: " ++ t } (evalTerm us m e)

cmpOp :: CmpOp -> Int -> Int -> Bool
cmpOp o = case o of
  Le -> (<=)
  Lt -> (<)
  Ge -> (>=)
  Gt -> (>)
  Eq -> (==)
  Ne -> (/=)
  Elem -> error "cmpOp"

evalTerm :: UserTypes -> Model -> Env -> Term -> TermVal
evalTerm us m e t = case t of
  Term card s -> case eval us m e s of
    Left f -> Error f
    Right v -> case card of
      Card -> VNum $ vSize v
      TheSet -> VTerm v
  EmptySet -> VEmptySet
  Num n -> VNum n

eval :: UserTypes -> Model -> Env -> Set -> Either String Value
eval us m e = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o v1 v2 -> let
      t1 = ppSet s1
      t2 = ppSet s2 in case (v1, v2) of
    (Left _, _) -> v1
    (_, Left _) -> v2
    (Right r1, Right r2) -> case o of
      Operations b -> let stOps = stUnOp o in case (r1, r2) of
        (Ints is, Ints os) -> Right . Ints $ IntSet.unions
          [Map.findWithDefault IntSet.empty (r, ob) $ opsMap m
            | r <- let rs = case mBaseType us s1 of
                         Just U -> apply m "Ur" is
                         _ -> is
                   in IntSet.toList $ if b == Star then apply m "j" rs else rs
            , ob <- IntSet.toList os]
        (VSet _, _) -> Left $ "unexpected set of set for "
          ++ stOps ++ " first argument: " ++ t1
        (_, VSet _) -> Left $ "unexpected set of set for "
          ++ stOps ++ " second argument: " ++ t2
      Minus -> case (r1, r2) of
        (VSet s, _) -> if r2 `Set.member` s then
           Right . VSet $ Set.delete r2 s else
           Left $ t2 ++ " is no member of nested set: " ++ t1
        (Ints is, Ints js) -> if js `IntSet.isSubsetOf` is
          && IntSet.size js == 1 then Right . Ints $ is IntSet.\\ js
          else Left $ t2 ++ " is no member of simple set : " ++ t1
        _ -> Left $ "unexpected minus set: " ++ t2
      _ -> if sameNesting r1 r2 then case (r1, r2) of
        (Ints is, Ints js) -> Right . Ints
          $ (if o == Inter then IntSet.intersection else IntSet.union) is js
        _ -> Right . VSet . (if o == Inter then Set.intersection else Set.union)
          (toVSet r1) $ toVSet r2
        else Left $ "incompatible set types: " ++ t1 ++ "versus: " ++ t2
  , foldUn = \ (UnOp _ s) o v -> let
      p = sUnOp (typeOfSet us s) o
      t = ppSet s in case v of
      Right (Ints is) -> case o of
        User _ Star -> Right . Ints . apply m p $ apply m "s" is
        Permissions Star
          | p == "Rp" -> Right . Ints . apply m p $ apply m "j" is
          | p == "Up" -> Right . Ints . apply m "Rp" . apply m "j"
              $ apply m "Ur" is
          | p == "Sp" -> Right . Ints . apply m "Rp" . apply m "j"
              $ apply m "Sr" is
        Permissions _
          | p == "Up" -> Right . Ints . apply m "Rp" $ apply m "Ur" is
          | p == "Sp" -> Right . Ints . apply m "Rp" $ apply m "Sr" is
        Roles Star -> Right . Ints . apply m (if p == "Pr" then "s" else "j")
          $ apply m p is
        AO -> if IntSet.null is then Left $ "empty set for AO: " ++ t
          else Right . Ints $ IntSet.deleteMax is
        OE -> if IntSet.null is then Left $ "empty set for OE: " ++ t
          else Right . Ints . IntSet.singleton $ IntSet.findMax is
        _ -> Right . Ints $ apply m p is
      Right (VSet vs) -> case o of
        AO -> if Set.null vs then Left $ "empty set of set for AO: " ++ t
          else Right . VSet $ Set.deleteMax vs
        OE -> if Set.null vs then Left $ "empty set of set for OE: " ++ t
          else Right $ Set.findMax vs
        _ -> Left $ "unexpected set of set for " ++ stUnOp o ++ ": " ++ t
      _ -> v
  , foldPrim = evalPrim m e }

evalPrim :: Model -> Env -> Set -> Either String Value
evalPrim m e s = case s of
  PrimSet p -> case Map.lookup p $ userSets m of
    Just (_, v, _) -> Right v
    Nothing -> case Map.lookup p $ strMap m of
      Just i -> Right . Ints $ IntSet.singleton i
      Nothing ->
        let ps = filter ((== p) . pStr_) . Set.toList $ permissions m in
        if null ps then Left $ "unknown set: " ++ p
        else Right . toInts m $ map pStr ps
  Var v@(MkVar i _ _) -> case IntMap.lookup i e of
    Just r -> Right r
    Nothing -> Left $ "unknown variable: " ++ stVar v
  _ -> error "evalPrim"

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
