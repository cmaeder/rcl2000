module Rcl.Check (properStructure, checkAccess) where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.Monoid (All (..))
import qualified Data.Set as Set

import Rcl.Ast (Base, SetType (..), baseType, primTypes)
import Rcl.Data

properSessions :: Model -> Bool
properSessions m =
  all (Set.null . illegalActiveRoles m) . Map.elems $ sessions m

nonCyclicRH :: Map.Map R (Set.Set R) -> Bool
nonCyclicRH m =
  all (Set.null . rhCycle m) $ Map.keys m

properStructure :: Model -> Bool
properStructure m = let
  us = users m
  rs = roles m
  ss = Map.elems $ sessions m
  ps = permissions m
  uas = ua m
  pas = pa m
  h = rh m
  i = inv m
  uSets = userSets m
  vs = Map.elems uSets
  im = intMap m
  sm = strMap m
  strs = Map.keysSet sm
  is = IntMap.keysSet im
  in all ((`Set.member` us) . user) ss
  && all ((`Set.isSubsetOf` rs) . activeRoles) ss
  && Set.map op ps `Set.isSubsetOf` operations m
  && Set.map obj ps `Set.isSubsetOf` objects m
  && Set.map fst uas `Set.isSubsetOf` us
  && Set.map snd uas `Set.isSubsetOf` rs
  && Set.map fst pas `Set.isSubsetOf` ps
  && Set.map snd pas `Set.isSubsetOf` rs
  && properSessions m
  && Map.keysSet h `Set.isSubsetOf` rs
  && all (`Set.isSubsetOf` rs) (Map.elems h)
  && nonCyclicRH h
  && Map.keysSet i `Set.isSubsetOf` rs
  && all (`Set.isSubsetOf` rs) (Map.elems i)
  && nonCyclicRH i
  && getAll (Map.foldMapWithKey (\ s js ->
       All . all ((s `Set.member`) . flip (Map.findWithDefault Set.empty) i)
       $ Set.toList js) h)
  && getAll (Map.foldMapWithKey (\ s js ->
       All . all ((s `Set.member`) . flip (Map.findWithDefault Set.empty) h)
       $ Set.toList js) i)
  && all (all checkValue . Map.toList) vs
  && all (all (\ (t, (v, _)) -> checkInts m (baseType t) $ getInts v)
         . Map.toList) vs
  && strs == getAllStrings m
  && strs == Set.fromList (IntMap.elems im)
  && is == IntSet.fromList (Map.elems sm)
  && IntSet.size is == Set.size strs
  && maybe True ((< next m) . fst) (IntSet.maxView is)

checkValue :: (SetType, (Value, a)) -> Bool
checkValue p = case p of
  (SetOf (ElemTy _), (Ints _, _)) -> True
  (SetOf e@(SetOf _), (VSet s, a)) ->
    all (\ v -> checkValue (e, (v, a))) $ Set.toList s
  (ElemTy _, (Ints vs, _)) -> IntSet.size vs == 1
  _ -> False

getInts :: Value -> IntSet.IntSet
getInts v = case v of
  Ints is -> is
  VSet s -> IntSet.unions . map getInts $ Set.toList s

getAllStrings :: Model -> Set.Set String
getAllStrings m =
  Set.unions $ Map.keysSet (userSets m) : map (getStrings m . fst) primTypes

checkInts :: Model -> Base -> IntSet.IntSet -> Bool
checkInts m b =
  all (\ i -> toStr i m `Set.member` getStrings m b) . IntSet.toList

permsOfRs :: Model -> Set.Set R -> Set.Set P
permsOfRs m = Set.unions . map (permissionsOfR m) . Set.toList

checkAccess :: Model -> String -> String -> String -> [String]
checkAccess m s oP oBj = let
  p = strP oP oBj
  ps = pStr p
  o1 = op p `Set.member` operations m
  o2 = obj p `Set.member` objects m
  pc = p `Set.member` permissions m
  errs = map snd $ filter fst
    [ (not o1, "unknown operation: " ++ oP)
    , (not o2, "unknown object: " ++ oBj)
    , (o1 && o2 && not pc, "unknown permission: " ++ ps)]
  in case Map.lookup s $ sessions m of
    Nothing -> ("unknown session: " ++ s) : errs
    Just (Session u@(Name n) as) -> if pc then
        if p `elem` permsOfRs m as then []
        else let
          ru = rolesOfR (rh m) $ rolesOfU m u
          rs = rolesOfR (inv m) $ rolesOfP m p
          rl = Set.toList rs
          nr = null rl
          nn = not nr
          us = map name . Set.toList . Set.unions $ map (usersOfR m) rl
          usr = null us
          usn = not usr in if p `elem` permsOfRs m ru
          then ["roles of user '" ++ n ++ "' not activated: " ++
            unwords (map role . Set.toList $ Set.intersection rs ru)]
          else ("missing assigned roles for user: " ++ n) :
            map snd (filter fst
            [ (nr, "no roles for permission: " ++ ps)
            , (nn, "required roles: " ++ unwords (map role rl))
            , (nn && usr, "no user with roles for permission: " ++ ps)
            , (usn, "possible users: " ++ unwords us)])
       else errs
