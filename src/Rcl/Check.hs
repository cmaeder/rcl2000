module Rcl.Check (properStructure, checkAccess) where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (isSubsetOf)

import Rcl.Ast
import Rcl.Data

properSessions :: Model -> Bool
properSessions m =
  all (\ s -> activeRoles s `isSubsetOf` rolesOfU m (user s))
  . Map.elems $ sessions m

nonCyclicRH :: Map R (Set.Set R) -> Bool
nonCyclicRH m =
  all (\ k -> Set.notMember k $ juniors m Set.empty k) $ Map.keys m

properStructure :: Model -> Bool
properStructure m = let
  us = users m
  rs = roles m
  ss = Map.elems $ sessions m
  ps = permissions m
  uas = ua m
  pas = pa m
  h = rh m
  uSets = userSets m
  vs = Map.elems uSets
  ks = Map.keys uSets
  im = intMap m
  sm = strMap m
  strs = Map.keysSet sm
  is = IntMap.keysSet im
  in all ((`Set.member` us) . user) ss
  && all (null . strToBase m) ks
  && all ((<= 1) . length . strToBase m) (Map.keys sm)
  && all ((`isSubsetOf` rs) . activeRoles) ss
  && Set.map op ps `isSubsetOf` operations m
  && Set.map obj ps `isSubsetOf` objects m
  && Set.map fst uas `isSubsetOf` us
  && Set.map snd uas `isSubsetOf` rs
  && Set.map fst pas `isSubsetOf` ps
  && Set.map snd pas `isSubsetOf` rs
  && properSessions m
  && Map.keysSet h `isSubsetOf` rs
  && all (`isSubsetOf` rs) (Map.elems h)
  && nonCyclicRH h
  && all checkValue vs
  && all (\ (t, v) -> checkInts m (baseType t) $ getInts v) vs
  && strs == getAllStrings m
  && strs == Set.fromList (IntMap.elems im)
  && is == IntSet.fromList (Map.elems sm)
  && IntSet.size is == Set.size strs
  && maybe True ((< next m) . fst) (IntSet.maxView is)

checkValue :: (SetType, Value) -> Bool
checkValue p = case p of
  (Set (ElemTy _), Ints _) -> True
  (Set e@(Set _), VSet s) -> all (curry checkValue e) $ Set.toList s
  (ElemTy _, Ints vs) -> IntSet.size vs == 1
  _ -> False

baseType :: SetType -> Base
baseType s = case s of
  ElemTy b -> b
  Set e -> baseType e

getInts :: Value -> IntSet
getInts v = case v of
  Ints is -> is
  VSet s -> IntSet.unions . map getInts $ Set.toList s

getAllStrings :: Model -> Set.Set String
getAllStrings m =
  Set.unions $ Map.keysSet (userSets m) : map (getStrings m) primTypes

checkInts :: Model -> Base -> IntSet -> Bool
checkInts m b =
  all (\ i -> toStr i m `Set.member` getStrings m b) . IntSet.toList

permsOfRs :: Model -> Set.Set R -> Set.Set P
permsOfRs m = Set.unions . map (permissionsOfR m) . Set.toList

checkAccess :: Model -> String -> OP -> OBJ -> Maybe String
checkAccess m s op1@(Operation oP) obj2@(Object oBj) =
  let p = strP oP oBj
      ps = pStr p
      rs = rolesOfP m p
      rl = Set.toList rs
      nr = null rl
      nn = not nr
      us = map name . Set.toList . Set.unions $ map (usersOfR m) rl
      usr = null us
      usn = not usr
      l = [(op1 `Set.notMember` operations m, "unknown operation: " ++ oP)
        , (obj2 `Set.notMember` objects m, "unknown object: " ++ oBj)
        , (p `Set.notMember` permissions m, "unknown permission: " ++ ps)
        , (nr, "no roles for permission: " ++ ps)
        , (nn, "required one of role: " ++ unwords (map role rl))
        , (nn && usr, "no user with roles for permission: " ++ ps)
        , (usn, "allowed for one of user: " ++ unwords us)]
      errs = unlines (map snd $ filter fst l)
  in case Map.lookup s $ sessions m of
    Nothing -> Just $ errs ++ "unknown session: " ++ s
    Just (Session u as) -> if p `elem` permsOfRs m as then Nothing
      else let ru = rolesOfU m u in if p `elem` permsOfRs m ru
         then Just $ "user roles not activated: " ++
           unwords (map role . Set.toList $ Set.intersection rs ru)
         else Just $ errs ++ "missing assigned roles for user: " ++ name u