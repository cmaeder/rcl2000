module Rcl.Model (initModel, addS, addU, addP, addR, toInts) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Data

sessionsOfU :: Model -> U -> Map String S
sessionsOfU m u = Map.filter ((== u) . user) $ sessions m

getRoles :: Map R (Set.Set R) -> R -> Set.Set R
getRoles m r = Set.insert r $ juniors m Set.empty r

insUserSet :: String -> Base -> [String] -> Model -> Model
insUserSet s b l m = addS s m
  { userSets = Map.insert s (Set $ ElemTy b, toInts m l) $ userSets m }

toInts :: Model -> [String] -> Value
toInts m = Ints . IntSet.fromList . map (toInt m)

toInt :: Model -> String -> Int
toInt m v = Map.findWithDefault (error $ "toInt: " ++ v) v $ strMap m

addS :: String -> Model -> Model
addS s m = let
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
addU u m = addS u m { users = Set.insert (Name u) $ users m }

addR :: String -> Model -> Model
addR r m = addS r m { roles = Set.insert (Role r) $ roles m }

addOp :: String -> Model -> Model
addOp o m = addS o m
  { operations = Set.insert (Operation o) $ operations m }

addObj :: String -> Model -> Model
addObj o m = addS o m { objects = Set.insert (Object o) $ objects m }

-- op and obj
addP :: String -> String -> Model -> Model
addP oP oBj m = addS (unwords [oP, oBj]) . addObj oBj $ addOp oP m
  { permissions = Set.insert (strP oP oBj) $ permissions m }

initModel :: Model -> Model
initModel = flip (foldr initFctMap) fcts . initOpsMap . initBases

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
