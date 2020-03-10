module Rcl.Model (initModel, addS, addU, checkU, addP, addR, checkR
  , toInts) where

import Data.Char (isAlphaNum, isAscii, isLetter)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (isPrefixOf)
import qualified Data.Set as Set

import Rcl.Ast (UnOp (..), SetType (..), Base (..), primTypes, unOps, stUnOp)
import Rcl.Data

sessionsOfU :: Model -> U -> Map String S
sessionsOfU m u = Map.filter ((== u) . user) $ sessions m

getRoles :: Map R (Set.Set R) -> R -> Set.Set R
getRoles m r = Set.insert r $ juniors m Set.empty r

insUserSet :: String -> Base -> [String] -> Model -> Model
insUserSet s b l m = addStr s m
  { userSets = Map.insert s (Set $ ElemTy b, toInts m l, l) $ userSets m }

toInts :: Model -> [String] -> Value
toInts m = Ints . IntSet.fromList . map (toInt m)

toInt :: Model -> String -> Int
toInt m v = Map.findWithDefault (error $ "toInt: " ++ v) v $ strMap m

-- | keywords for use (cf. use-mode.el)
keywords :: Set.Set String
keywords = Set.fromList $ let
  l = map show primTypes
  l1 = map ("SetOf" ++) l
  l2 = map ('A' :) l1
  in l ++ l1 ++ l2 ++ words "RH UA PA SessionRoles Builtin RBAC \
  \ model enum class attributes operations constraints begin end inv \
  \ pre post association aggregation between composition role init \
  \ for in if then declare insert delete destroy new into from do let \
  \ context abstract associationclass ordered else endif \
  \ Real Integer Boolean Collection String OrderedSet Set Bag Sequence \
  \ result self"
  ++ map stUnOp unOps

addS :: String -> Model -> Model
addS s m = let
  b = all (\ c -> isAscii c && isAlphaNum c || c == '_') s
  k = s `Set.member` keywords
  p = any (`isPrefixOf` s) $ words "SetOfSet ASetOfSet"
  sm = strMap m
  in case Map.lookup s sm of
    Nothing
      | not b -> error $ "illegal character in: " ++ s
      | k -> error $ "illegal RBAC or use keyword: " ++ s
      | p -> error $ "illegal prefix for: " ++ s
      | null s -> error "addS: illegal empty string"
      | not . isLetter $ head s ->
        error $ "string must start with a letter: " ++ s
      | otherwise -> addStr s m
    _ -> m

addStr :: String -> Model -> Model
addStr s m = let i = next m in m
  { strMap = Map.insert s i $ strMap m
  , intMap = IntMap.insert i s $ intMap m
  , next = i + 1 }

addU :: String -> Model -> Model
addU u m = addS u m { users = Set.insert (Name u) $ users m }

checkU :: String -> Model -> Bool
checkU u m = Name u `Set.member` users m

addR :: String -> Model -> Model
addR r m = addS r m { roles = Set.insert (Role r) $ roles m }

checkR :: String -> Model -> Bool
checkR r m = Role r `Set.member` roles m

addOp :: String -> Model -> Model
addOp o m = addS o m
  { operations = Set.insert (Operation o) $ operations m }

addObj :: String -> Model -> Model
addObj o m = addS o m { objects = Set.insert (Object o) $ objects m }

-- op and obj
addP :: String -> String -> Model -> Model
addP oP oBj m = let
  p = strP oP oBj
  ps = permissions m
  in if p `Set.member` ps
    then error $ "permission already known: " ++ oP ++ " " ++ oBj
    else addS (pStr p) . addObj oBj $ addOp oP m
      { permissions = Set.insert p ps }

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
