module Rcl.Model where

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
import Rcl.Type

newtype U = Name { name :: String } deriving (Eq, Ord, Show)
newtype R = Role { role :: String } deriving (Eq, Ord, Show)
newtype OP = Operation { operation :: String } deriving (Eq, Ord, Show)
newtype OBJ = Object { object :: String } deriving (Eq, Ord, Show)
data S = Session { user :: U, activeRoles :: Set.Set R }
  deriving (Eq, Ord, Show)
data P = Permission { op :: OP, obj :: OBJ }
  deriving (Eq, Ord, Show)

pStr :: P -> String
pStr p = unwords [operation $ op p, object $ obj p]

strP :: String -> String -> P
strP u v = Permission (Operation u) $ Object v

strP1 :: String -> P
strP1 s = case words s of
  [u, v] -> strP u v
  _ -> error $ "strP1: " ++ s

data Model = Model
  { roles :: Set.Set R
  , users :: Set.Set U
  , operations :: Set.Set OP
  , objects :: Set.Set OBJ
  , permissions :: Set.Set P
  , sessions :: Map String S
  , userSets :: Map String (SetType, Value)
  , ua :: Set.Set (U, R)
  , pa :: Set.Set (P, R)
  , rh :: Map R (Set.Set R) -- direct junior roles
  , strMap :: Map String Int
  , intMap :: IntMap String
  , fctMap :: Map String (IntMap IntSet)
  , opsMap :: Map (Int, Int) IntSet -- operations
  , next :: Int } -- next unused Int

data Value = Ints IntSet | VSet (Set.Set Value) deriving (Eq, Ord, Show)

emptyModel :: Model
emptyModel = Model
  { roles = Set.empty
  , users = Set.empty
  , operations = Set.empty
  , objects = Set.empty
  , permissions = Set.empty
  , sessions = Map.empty
  , userSets = Map.empty
  , ua = Set.empty
  , pa = Set.empty
  , rh = Map.empty
  , strMap = Map.empty
  , intMap = IntMap.empty
  , fctMap = Map.empty
  , opsMap = Map.empty
  , next = 1 }

rolesOfU :: Model -> U -> Set.Set R
rolesOfU m u = Set.foldr
  (\ (v, r) -> if u == v then Set.insert r else id) Set.empty $ ua m

rolesOfP :: Model -> P -> Set.Set R
rolesOfP m p = Set.foldr
  (\ (v, r) -> if p == v then Set.insert r else id) Set.empty $ pa m

rolesOfS :: Model -> String -> Set.Set R
rolesOfS m s = case Map.lookup s $ sessions m of
  Just (Session _ rs) -> rs
  Nothing -> error $ "rolesOfS:" ++ s

close :: Model -> Set.Set R -> Set.Set R
close m = Set.unions . map (getRoles $ rh m) . Set.toList

userOfS :: Model -> String -> U
userOfS m s = case Map.lookup s $ sessions m of
  Just (Session u _) -> u
  Nothing -> error $ "rolesOfS:" ++ s

usersOfR :: Model -> R -> Set.Set U
usersOfR m r = Set.foldr
  (\ (u, v) -> if r == v then Set.insert u else id) Set.empty $ ua m

sessionsOfU :: Model -> U -> Map String S
sessionsOfU m u = Map.filter ((== u) . user) $ sessions m

permissionsOfR :: Model -> R -> Set.Set P
permissionsOfR m r = Set.foldr
  (\ (p, v) -> if r == v then Set.insert p else id) Set.empty $ pa m

operationsOf :: Model -> (R, OBJ) -> Set.Set OP
operationsOf m ro = Set.foldr (\ (Permission oP oBj, v) ->
  if ro == (v, oBj) then Set.insert oP else id) Set.empty $ pa m

objectsOfP :: Set.Set P -> Set.Set OBJ
objectsOfP = Set.map obj

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

updUserSet :: String -> Base -> [String] -> Model -> Model
updUserSet s b l m =
  assert (all (`Set.member` getStrings m b) l)
  $ insUserSet s b l m

updUserSet2 :: String -> Base -> [[String]] -> Model -> Model
updUserSet2 s b ll m =
  assert (all (all (`Set.member` getStrings m b)) ll)
  m { userSets = Map.insert s
      (Set . Set $ ElemTy b, VSet . Set.fromList $ map
        (toInts m) ll)
    $ userSets m }

updUserSet3 :: String -> Base -> [[[String]]] -> Model -> Model
updUserSet3 s b lll m =
  assert (all (all (all (`Set.member` getStrings m b))) lll)
  m { userSets = Map.insert s
      (Set . Set . Set $ ElemTy b, VSet . Set.fromList $ map
        (VSet . Set.fromList . map (toInts m)) lll)
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

-- sid and user
addS :: String -> String -> Model -> Model
addS s u m = addString s $ addU u m
  { sessions = Map.insert s (Session (Name u) Set.empty) $ sessions m }

-- sid and active role
addSR :: String -> String -> Model -> Model
addSR s r m = let sm = sessions m in addR r m
  { sessions = case Map.lookup s sm of
      Just (Session u rs) ->
        Map.insert s (Session u $ Set.insert (Role r) rs) sm
      Nothing -> error $ "addSR:" ++ s }

addSRs :: String -> [String] -> Model -> Model
addSRs s rs m = let sm = sessions m in (foldr addR m rs)
  { sessions = case Map.lookup s sm of
      Just (Session u _) ->
        Map.insert s (Session u . Set.fromList $ map Role rs) sm
      Nothing -> error $ "addSRs:" ++ s }

addSURs :: String -> String -> [String] -> Model -> Model
addSURs s u rs m = addU u (foldr addR m rs)
  { sessions = Map.insert s (Session (Name u) . Set.fromList $ map Role rs)
    $ sessions m }

-- user and role
addUA :: String -> String -> Model -> Model
addUA u r m = addU u $ addR r m { ua = Set.insert (Name u, Role r) $ ua m }

addPA :: String -> String -> String -> Model -> Model
addPA oP oBj r m = addP oP oBj $ addR r m
  { pa = Set.insert (strP oP oBj, Role r) $ pa m }

addRH :: String -> [String] -> Model -> Model
addRH r js m = addR r (foldr addR m js)
    { rh = let nh = Map.insert (Role r) (Set.fromList $ map Role js) $ rh m
      in assert (nonCyclicRH nh) nh }

-- | short strings for fctMap
sUnOp :: Maybe SetType -> UnOp -> String
sUnOp t o = let u = take 1 $ show o
  in case o of
  User -> if t == Just (ElemTy S) then "us" else "U"
  Objects -> "Ob"
  Roles _ -> case t >>= \ s -> if isElem s then Just s else elemType s of
      Just (ElemTy r) -> case r of
        U -> "ur"
        P -> "pr"
        S -> "sr"
        _ -> u
      _ -> u
  _ -> u

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
