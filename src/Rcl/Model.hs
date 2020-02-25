module Rcl.Model where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set ((\\), isSubsetOf)

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import Rcl.Ast

newtype U = User { name :: String } deriving (Eq, Ord, Show)
newtype R = Role { role :: String } deriving (Eq, Ord, Show)
newtype OP = Operation { operation :: String } deriving (Eq, Ord, Show)
newtype OBJ = Object { object :: String } deriving (Eq, Ord, Show)
data S = Session { sid :: String, user :: U, activeRoles :: Set.Set R }
  deriving (Eq, Ord, Show)
data P = Permission { op :: OP, obj :: OBJ }
  deriving (Eq, Ord, Show)

pStr :: P -> String
pStr p = unwords [operation $ op p, object $ obj p]

strP :: String -> P
strP s = case words s of
  [u, v] -> Permission (Operation u) $ Object v
  _ -> error $ "strP: " ++ s

data Model = Model
  { roles :: Set.Set R
  , users :: Set.Set U
  , operations :: Set.Set OP
  , objects :: Set.Set OBJ
  , permissions :: Set.Set P
  , sessions :: Set.Set S
  , userSets :: Map String (SetType, Value)
  , ua :: Set.Set (U, R)
  , pa :: Set.Set (P, R)
  , rh :: Map R (Set.Set R) -- direct junior roles
  , strMap :: Map String Int
  , intMap :: Map Int String
  , next :: Int -- next unused Int
  }

data Value = VInt Int | VSet (Set.Set Value) deriving (Eq, Ord, Show)

rolesOfU :: Model -> U -> Set.Set R
rolesOfU sets u =
  Set.foldr (\ (v, r) -> if u == v then Set.insert r else id) Set.empty
  $ ua sets

rolesOfP :: Model -> P -> Set.Set R
rolesOfP sets p =
  Set.foldr (\ (v, r) -> if p == v then Set.insert r else id) Set.empty
  $ pa sets

properSessions :: Model -> Bool
properSessions sets =
  all (\ s -> activeRoles s `isSubsetOf` rolesOfU sets (user s))
  . Set.toList $ sessions sets

juniors :: Map R (Set.Set R) -> Set.Set R -> R -> Set.Set R
juniors m visited r =
  let s = Map.findWithDefault Set.empty r m \\ visited
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
  ss = sessions m
  ps = permissions m
  uas = ua m
  pas = pa m
  h = rh m
  vs = Map.elems $ userSets m
  im = intMap m
  sm = strMap m
  strs = Map.keysSet sm
  is = Map.keysSet im
  in Set.map user ss `isSubsetOf` us
  && Set.unions (map activeRoles $ Set.toList ss) `isSubsetOf` rs
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
  && strs == Set.fromList (Map.elems im)
  && is == Set.fromList (Map.elems sm)
  && maybe True (< next m) (Set.lookupMax is)

checkValue :: (SetType, Value) -> Bool
checkValue p = case p of
  (Set e, VSet s) -> all (curry checkValue e) $ Set.toList s
  (ElemTy _, VInt _) -> True
  _ -> False

baseType :: SetType -> Base
baseType s = case s of
  ElemTy b -> b
  Set e -> baseType e

getInts :: Value -> IntSet
getInts v = case v of
  VInt i -> IntSet.singleton i
  VSet s -> IntSet.unions . map getInts $ Set.toList s

getStrings :: Model -> Base -> Set.Set String
getStrings m b = case b of
  U -> Set.map name $ users m
  R -> Set.map role $ roles m
  OBJ -> Set.map object $ objects m
  OP -> Set.map operation $ operations m
  P -> Set.map sid $ sessions m
  S -> Set.map pStr $ permissions m

getAllStrings :: Model -> Set.Set String
getAllStrings m = Set.unions $ map (getStrings m) primTypes

checkInts :: Model -> Base -> IntSet -> Bool
checkInts m b is =
  all (\ i -> Set.member
      (Map.findWithDefault "" i $ intMap m)
      $ getStrings m b) $ IntSet.toList is
