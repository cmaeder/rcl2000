module Rcl.Model (initModel, addU, addP, addR, toInts) where

import Control.Exception (assert)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set ((\\), isSubsetOf)

import Rcl.Ast
import Rcl.Data

rolesOfU :: Model -> U -> Set.Set R
rolesOfU m u = Set.foldr
  (\ (v, r) -> if u == v then Set.insert r else id) Set.empty $ ua m

rolesOfP :: Model -> P -> Set.Set R
rolesOfP m p = Set.foldr
  (\ (v, r) -> if p == v then Set.insert r else id) Set.empty $ pa m

usersOfR :: Model -> R -> Set.Set U
usersOfR m r = Set.foldr
  (\ (u, v) -> if r == v then Set.insert u else id) Set.empty $ ua m

sessionsOfU :: Model -> U -> Map String S
sessionsOfU m u = Map.filter ((== u) . user) $ sessions m

permissionsOfR :: Model -> R -> Set.Set P
permissionsOfR m r = Set.foldr
  (\ (p, v) -> if r == v then Set.insert p else id) Set.empty $ pa m

properSessions :: Model -> Bool
properSessions m =
  all (\ s -> activeRoles s `isSubsetOf` rolesOfU m (user s))
  . Map.elems $ sessions m

juniors :: Map R (Set.Set R) -> Set.Set R -> R -> Set.Set R
juniors m visited r = let s = Map.findWithDefault Set.empty r m \\ visited
  in if Set.null s then s else Set.union s
     . Set.unions . map (juniors m $ Set.insert r visited) $ Set.toList s

getRoles :: Map R (Set.Set R) -> R -> Set.Set R
getRoles m r = Set.insert r $ juniors m Set.empty r

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
  vs = Map.elems $ userSets m
  im = intMap m
  sm = strMap m
  strs = Map.keysSet sm
  is = IntMap.keysSet im
  in all ((`Set.member` us) . user) ss
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

getStrings :: Model -> Base -> Set.Set String
getStrings m b = case b of
  U -> Set.map name $ users m
  R -> Set.map role $ roles m
  OBJ -> Set.map object $ objects m
  OP -> Set.map operation $ operations m
  S -> Map.keysSet $ sessions m
  P -> Set.map pStr $ permissions m

getAllStrings :: Model -> Set.Set String
getAllStrings m = Set.unions $ map (getStrings m) primTypes

checkInts :: Model -> Base -> IntSet -> Bool
checkInts m b is =
  all (\ i -> Set.member
      (IntMap.findWithDefault "" i $ intMap m)
      $ getStrings m b) $ IntSet.toList is

insUserSet :: String -> Base -> [String] -> Model -> Model
insUserSet s b l m =
  m { userSets = Map.insert s
      (Set $ ElemTy b, toInts m l)
    $ userSets m }

toInts :: Model -> [String] -> Value
toInts m = Ints . IntSet.fromList . map (toInt m)

toInt :: Model -> String -> Int
toInt m v = Map.findWithDefault 0 v $ strMap m

addString :: String -> Model -> Model
addString s m = let
  sm = strMap m
  im = intMap m
  i = next m
  in case Map.lookup s sm of
    Nothing -> m {
      strMap = Map.insert s i sm
      , intMap = IntMap.insert i s im
      , next = i + 1 }
    _ -> m

addU :: String -> Model -> Model
addU u m = addString u m { users = Set.insert (Name u) $ users m }

addR :: String -> Model -> Model
addR r m = addString r m { roles = Set.insert (Role r) $ roles m }

addOp :: String -> Model -> Model
addOp o m = addString o m
  { operations = Set.insert (Operation o) $ operations m }

addObj :: String -> Model -> Model
addObj o m = addString o m { objects = Set.insert (Object o) $ objects m }

-- op and obj
addP :: String -> String -> Model -> Model
addP oP oBj m = addString (unwords [oP, oBj]) . addObj oBj $ addOp oP m
  { permissions = Set.insert (strP oP oBj) $ permissions m }

initModel :: Model -> Model
initModel m = let n = flip (foldr initFctMap) fcts . initOpsMap $ initBases m
  in assert (properStructure n) n

-- | insert initial base sets
initBases :: Model -> Model
initBases m = foldr
  (\ b n -> insUserSet (show b) b (Set.toList $ getStrings m b) n)
  m primTypes

initOpsMap :: Model -> Model
initOpsMap m =
  m { opsMap = Set.foldr
    (\ (Permission (Operation oP) (Object oBj), Role r) n ->
    let p = (toInt m r, toInt m oBj)
        is = Map.findWithDefault IntSet.empty p n
    in Map.insert p (IntSet.insert (toInt m oP) is) n) Map.empty $ pa m }

fcts :: [(Base, UnOp)]
fcts = map toR [U, P, S, R] ++
  [(S, User), (R, User), (U, Sessions), (R, Permissions True), (P, Objects)]

toR :: Base -> (Base, UnOp)
toR b = (b, Roles True)

initFctMap :: (Base, UnOp) -> Model -> Model
initFctMap p@(b, o) m = m
  { fctMap = Map.insert (sUnOp (Just $ ElemTy b) o) (function p m) $ fctMap m }

function :: (Base, UnOp) -> Model -> IntMap IntSet
function bo m = let
  ss = Map.toList $ sessions m
  rs = Set.toList $ roles m
  us = Set.toList $ users m
  ps = Set.toList $ permissions m
  in case bo of
  (S, User) -> IntMap.fromList $ map (\ (s, Session (Name u) _) ->
    (toInt m s, IntSet.singleton (toInt m u))) ss
  (_, User) -> IntMap.fromList $ map (\ r ->
        (toInt m $ role r, IntSet.fromList $ map (toInt m . name)
        . Set.toList $ usersOfR m r)) rs
  (U, Roles _) -> IntMap.fromList $ map (\ u ->
        (toInt m $ name u, IntSet.fromList . map (toInt m . role)
        . Set.toList $ rolesOfU m u)) us
  (P, Roles _) -> IntMap.fromList $ map (\ p ->
        (toInt m $ pStr p, IntSet.fromList $ map (toInt m . role)
        . Set.toList $ rolesOfP m p)) ps
  (S, Roles _) -> IntMap.fromList $ map (\ (s, Session _ as) ->
        (toInt m s, IntSet.fromList . map (toInt m . role)
        $ Set.toList as)) ss
  (_, Roles _) -> IntMap.fromList $ map (\ r ->
        (toInt m $ role r, IntSet.fromList . map (toInt m . role)
        . Set.toList $ getRoles (rh m) r)) rs
  (_, Sessions) -> IntMap.fromList $ map (\ u ->
        (toInt m $ name u, IntSet.fromList . map (toInt m)
        . Map.keys $ sessionsOfU m u)) us
  (_, Permissions _) -> IntMap.fromList $ map (\ r ->
        (toInt m $ role r, IntSet.fromList . map (toInt m . pStr)
        . Set.toList $ permissionsOfR m r)) rs
  (_, Objects) -> IntMap.fromList $ map (\ p@(Permission _ (Object ob)) ->
        (toInt m $ pStr p, IntSet.singleton (toInt m ob))) ps
  _ -> error "function"
