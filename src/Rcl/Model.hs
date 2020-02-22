module Rcl.Model where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set, (\\), isSubsetOf)

newtype U = U String deriving (Eq, Ord, Show)
newtype R = R String deriving (Eq, Ord, Show)
newtype OP = OP String deriving (Eq, Ord, Show)
newtype OBJ = OBJ String deriving (Eq, Ord, Show)
data S = S { sid :: String, user :: U, activeRoles :: Set R }
  deriving (Eq, Ord, Show)
data P = P { op :: OP, object :: OBJ }
  deriving (Eq, Ord, Show)

data Model = Model
  { roles :: Set R
  , users :: Set U
  , operations :: Set OP
  , objects :: Set OBJ
  , permissions :: Set P
  , sessions :: Set S
  , cr :: Set (Set R)
  , cp :: Set (Set P)
  , cu :: Set (Set U)
  , ua :: Set (U, R)
  , pa :: Set (P, R)
  , rh :: Map R (Set R) -- direct junior roles
  }

rolesOfU :: Model -> U -> Set R
rolesOfU sets u =
  Set.foldr (\ (v, r) -> if u == v then Set.insert r else id) Set.empty
  $ ua sets

rolesOfP :: Model -> P -> Set R
rolesOfP sets p =
  Set.foldr (\ (v, r) -> if p == v then Set.insert r else id) Set.empty
  $ pa sets

properSessions :: Model -> Bool
properSessions sets =
  all (\ s -> activeRoles s `isSubsetOf` rolesOfU sets (user s))
  . Set.toList $ sessions sets

juniors :: Map R (Set R) -> Set R -> R -> Set R
juniors m visited r =
  let s = Map.findWithDefault Set.empty r m \\ visited
  in if Set.null s then s else Set.union s
     . Set.unions . map (juniors m $ Set.insert r visited) $ Set.toList s

getRoles :: Map R (Set R) -> R -> Set R
getRoles m r = Set.insert r $ juniors m Set.empty r

nonCyclicRH :: Map R (Set R) -> Bool
nonCyclicRH m =
  all (\ k -> Set.notMember k $ juniors m Set.empty k) $ Map.keys m

properStructure :: Model -> Bool
properStructure sets = let
  us = users sets
  rs = roles sets
  ss = sessions sets
  ps = permissions sets
  uas = ua sets
  pas = pa sets
  m = rh sets
  in Set.map user ss `isSubsetOf` us
  && Set.unions (map activeRoles $ Set.toList ss) `isSubsetOf` rs
  && Set.map op ps `isSubsetOf` operations sets
  && Set.map object ps `isSubsetOf` objects sets
  && Set.unions (Set.toList $ cr sets) `isSubsetOf` rs
  && Set.unions (Set.toList $ cu sets) `isSubsetOf` us
  && Set.unions (Set.toList $ cp sets) `isSubsetOf` ps
  && Set.map fst uas `isSubsetOf` us
  && Set.map snd uas `isSubsetOf` rs
  && Set.map fst pas `isSubsetOf` ps
  && Set.map snd pas `isSubsetOf` rs
  && properSessions sets
  && Map.keysSet m `isSubsetOf` rs
  && all (`isSubsetOf` rs) (Map.elems m)
  && nonCyclicRH m
