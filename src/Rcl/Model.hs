module Rcl.Model (addP, addR, addS, addSURs, addU, checkU, initModel, initRH,
  initSess, insSet) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Data

sessionsOfU :: Model -> U -> Map String S
sessionsOfU m u = Map.filter ((== u) . user) $ sessions m

getRoles :: Map R (Set.Set R) -> R -> Set.Set R
getRoles m r = Set.insert r $ Map.findWithDefault Set.empty r m

toList :: Map R (Set.Set R) -> [(R, R)]
toList = concatMap (\ (k, s) -> map (\ e -> (k, e)) $ Set.toList s) . Map.toList

fromList :: [(R, R)] -> Map R (Set.Set R)
fromList = foldr
  (\ (r1, r2) -> Map.insertWith Set.union r1 $ Set.singleton r2) Map.empty

invert :: Map R (Set.Set R) -> Map R (Set.Set R)
invert = fromList . map (\ (a, b) -> (b, a)) . toList

insSet :: String -> SetType -> Value -> [String] -> Model -> Model
insSet s k v l m = let
  u = userSets m
  r = (v, l) in m
  { userSets = Map.insert s (case Map.lookup s u of
      Nothing -> Map.singleton k r
      Just e -> Map.insert k r e) u }

insUserSet :: (Base, String) -> Model -> Model
insUserSet (b, s) m = let l = Set.toList $ getStrings m b in
  addS s $ insSet s (toSet b) (toInts m l) l m

insUserSetAux :: Base -> Model -> Model
insUserSetAux b m = case lookup b primTypes of
  Just s -> insUserSet (b, s) m
  _ -> error "insUserSetAux"

addS :: String -> Model -> Model
addS s m = if null s then error "addS" else case Map.lookup s $ strMap m of
    Nothing -> addStr s m
    _ -> m

addStr :: String -> Model -> Model
addStr s m = let i = next m in m
  { strMap = Map.insert s i $ strMap m
  , intMap = IntMap.insert i s $ intMap m
  , next = i + 1 }

addU :: String -> Model -> Model
addU u m = addS u m { users = Set.insert (Name u) $ users m }

addR :: String -> Model -> Model
addR r m = addS r m { roles = Set.insert (Role r) $ roles m }

addOp :: String -> Model -> Model
addOp o m = addS o m
  { operations = Set.insert (Operation o) $ operations m }

addObj :: String -> Model -> Model
addObj o m = addS o m { objects = Set.insert (Resource o) $ objects m }

-- op and obj
addP :: String -> String -> Model -> Model
addP oP oBj m = let p = strP oP oBj in addS (pStr p) . addObj oBj $ addOp oP m
  { permissions = Set.insert p $ permissions m }

checkU :: String -> Model -> Bool
checkU u m = Name u `Set.member` users m

addSURs :: String -> String -> [String] -> Model -> Either String Model
addSURs s u rs m = let sm = sessions m in
  if s `Map.member` sm then Left $ "session identifier already known: " ++ s
  else if checkU u m then let
    v = Session (Name u) . Set.fromList $ map Role rs
    is = illegalActiveRoles m v
    in if Set.null is then Right $ addS s (foldr addR m rs)
      { sessions = Map.insert s v sm }
      else Left $ "unassigned roles for user of session: "
        ++ unwords (s : u : map role (Set.toList is))
   else Left $ "user unknown: " ++ u

initModel :: Model -> Model
initModel = flip (foldr initFctMap) fcts . initOpsMap . initBases

initRH :: Model -> Model
initRH m = let
  n = transClosure $ rh m
  r = transReduce n in m { rh = n, inv = invert n, rhim = r, invim = invert r }

transReduce :: Map.Map R (Set.Set R) -> Map.Map R (Set.Set R)
transReduce m = Map.map ( \ s -> let
    d = Map.fromList . map (\ a -> (a, a)) $ Set.toList s
    in Set.filter ( \ j -> Map.null . Map.filter (Set.member j)
      . Map.intersection m $ Map.delete j d) s) m

-- | insert initial base sets
initBases :: Model -> Model
initBases = flip (foldr insUserSet) primTypes

initOpsMap :: Model -> Model
initOpsMap m = m { opsMap = Set.foldr
    (\ (Permission (Operation oP) (Resource oBj), Role r) n ->
    let p = (toInt m r, toInt m oBj)
        is = Map.findWithDefault IntSet.empty p n
    in Map.insert p (IntSet.insert (toInt m oP) is) n) Map.empty $ pa m }

fcts :: [(Base, UnOp)]
fcts = map toR [U, P, S] ++ [(S, User Singular TheOp), (R, User Plural Star)
  , (R, Roles TheOp), (U, Sessions), (R, Permissions Star), (P, Object Plural)]
  ++ [(R, Iors i b) | b <- [TheOp, Star], i <- [Jun, Sen]]

toR :: Base -> (Base, UnOp)
toR b = (b, Roles Star)

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
  (S, User _ _) -> IntMap.fromList $ map (\ (s, Session (Name u) _) ->
    (toInt m s, IntSet.singleton (toInt m u))) ss
  (_, User _ _) -> IntMap.fromList $ map (\ r ->
        (toInt m $ role r, IntSet.fromList . map (toInt m . name)
        . Set.toList . usersOfRs m $ Set.singleton r)) rs
  (U, Roles _) -> IntMap.fromList $ map (\ u ->
        (toInt m $ name u, IntSet.fromList . map (toInt m . role)
        . Set.toList $ rolesOfU m u)) us
  (P, Roles _) -> IntMap.fromList $ map (\ p ->
        (toInt m $ pStr p, IntSet.fromList . map (toInt m . role)
        . Set.toList $ rolesOfP m p)) ps
  (S, Roles _) -> IntMap.fromList $ map (\ (s, Session _ as) ->
        (toInt m s, IntSet.fromList . map (toInt m . role)
        $ Set.toList as)) ss
  (R, Iors i b) -> IntMap.fromList $ map (\ r ->
        (toInt m $ role r, IntSet.fromList . map (toInt m . role)
        . Set.toList $ case i of
            Jun -> if b == Star then getRoles (rh m) r
              else Map.findWithDefault Set.empty r $ rhim m
            Sen -> if b == Star then getRoles (inv m) r
              else Map.findWithDefault Set.empty r $ invim m)) rs
  (_, Sessions) -> IntMap.fromList $ map (\ u ->
        (toInt m $ name u, IntSet.fromList . map (toInt m)
        . Map.keys $ sessionsOfU m u)) us
  (_, Permissions _) -> IntMap.fromList $ map (\ r ->
        (toInt m $ role r, IntSet.fromList . map (toInt m . pStr)
        . Set.toList . permissionsOfRs m $ Set.singleton r)) rs
  (_, Object _) -> IntMap.fromList $ map (\ p@(Permission _ (Resource ob)) ->
        (toInt m $ pStr p, IntSet.singleton (toInt m ob))) ps
  _ -> error "function"

initSess :: Model -> Model
initSess = insUserSetAux S . flip (foldr initFctMap)
  [(S, Roles TheOp), (S, User Singular TheOp), (U, Sessions)]
