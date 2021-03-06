module Rcl.Data where

import qualified Data.IntMap as IntMap (IntMap, empty, findWithDefault)
import qualified Data.IntSet as IntSet (IntSet, fromList, toList)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast

newtype U = Name { name :: String } deriving (Eq, Ord, Show)
newtype R = Role { role :: String } deriving (Eq, Ord, Show)
newtype OP = Operation { operation :: String } deriving (Eq, Ord, Show)
newtype OBJ = Resource { resource :: String } deriving (Eq, Ord, Show)
data S = Session { user :: U, activeRoles :: Set.Set R }
  deriving (Eq, Ord, Show)
data P = Permission { op :: OP, obj :: OBJ }
  deriving (Eq, Ord, Show)

data Value = Ints IntSet.IntSet | VSet (Set.Set Value) deriving (Eq, Ord, Show)

data Model = Model
  { roles :: Set.Set R
  , users :: Set.Set U
  , operations :: Set.Set OP
  , objects :: Set.Set OBJ
  , permissions :: Set.Set P
  , sessions :: Map.Map String S
  , userSets :: Map.Map String (Map.Map SetType (Value, [String]))
  , ua :: Set.Set (U, R)
  , pa :: Set.Set (P, R)
  , up :: Set.Set (U, P)
  , rh :: Map.Map R (Set.Set R) -- all junior roles
  , inv :: Map.Map R (Set.Set R) -- inverse senior roles
  , rhim :: Map.Map R (Set.Set R) -- immediate junior roles
  , invim :: Map.Map R (Set.Set R) -- inverse immediate senior roles
  , strMap :: Map.Map String Int
  , intMap :: IntMap.IntMap String
  , fctMap :: Map.Map String (IntMap.IntMap IntSet.IntSet)
  , opsMap :: Map.Map (Int, Int) IntSet.IntSet -- operations
  , next :: Int } -- next unused Int
  deriving Show

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
  , up = Set.empty
  , rh = Map.empty
  , inv = Map.empty
  , rhim = Map.empty
  , invim = Map.empty
  , strMap = Map.empty
  , intMap = IntMap.empty
  , fctMap = Map.empty
  , opsMap = Map.empty
  , next = 1 }

getUserTypes :: Model -> UserTypes
getUserTypes = foldr
  (\ (n, m) -> Map.insertWith Set.union n $ Map.keysSet m)
  builtinTypes . Map.toList . userSets

pStrC :: Char -> P -> String
pStrC c p = operation (op p) ++ c : resource (obj p)

pStr :: P -> String
pStr = pStrC ' '

pStr_ :: P -> String
pStr_ = pStrC '_'

strP :: String -> String -> P
strP u v = Permission (Operation u) $ Resource v

usersOfRs :: Model -> Set.Set R -> Set.Set U
usersOfRs m r = Set.fold
  (\ (u, v) -> if v `Set.member` r then Set.insert u else id) Set.empty $ ua m

usersOfR :: Model -> R -> Set.Set U
usersOfR m = usersOfRs m . rolesOfR (inv m) . Set.singleton

rolesOfR :: Map.Map R (Set.Set R) -> Set.Set R -> Set.Set R
rolesOfR m s = Set.unions $ s
  : map (flip (Map.findWithDefault Set.empty) m) (Set.toList s)

rolesOfU :: Model -> U -> Set.Set R
rolesOfU m u = Set.fold
  (\ (v, r) -> if u == v then Set.insert r else id) Set.empty $ ua m

rolesOfP :: Model -> P -> Set.Set R
rolesOfP m p = Set.fold
  (\ (v, r) -> if p == v then Set.insert r else id) Set.empty $ pa m

permissionsOfRs :: Model -> Set.Set R -> Set.Set P
permissionsOfRs m r = Set.fold
  (\ (p, v) -> if v `Set.member` r then Set.insert p else id) Set.empty $ pa m

permissionsOfR :: Model -> R -> Set.Set P
permissionsOfR m = permissionsOfRs m . rolesOfR (rh m) . Set.singleton

juniors :: Map.Map R (Set.Set R) -> Set.Set R -> R -> Set.Set R
juniors m visited r = let s = Map.findWithDefault Set.empty r m
  in if Set.null s then s else Set.unions . (s :)
  . map (juniors m $ Set.insert r visited) . Set.toList $ s Set.\\ visited

transClosure :: Map.Map R (Set.Set R) -> Map.Map R (Set.Set R)
transClosure m = Map.mapWithKey (const . juniors m Set.empty) m

getStrings :: Model -> Base -> Set.Set String
getStrings m b = case b of
  U -> Set.map name $ users m
  R -> Set.map role $ roles m
  OBJ -> Set.map resource $ objects m
  OP -> Set.map operation $ operations m
  S -> Map.keysSet $ sessions m
  P -> Set.map pStr $ permissions m

-- | short strings for fctMap
sUnOp :: Maybe SetType -> UnOp -> String
sUnOp t o = let
  u = take 1 $ show o
  mt = t >>= \ s -> Just $ case s of
    ElemTy _ -> s
    SetOf e -> e
  in case o of
  User _ _ -> if t == Just (ElemTy S) then "u" else u
  Object _ -> "B"
  Roles _ -> case mt of
    Just (ElemTy r) -> show r ++ "r"
    _ -> u
  Permissions _ -> case mt of
    Just (ElemTy r) -> show r ++ "p"
    _ -> u
  Iors i b -> case i of
      Jun -> "j"
      Sen -> "s"
    ++ case b of
      Star -> ""
      TheOp -> "i" -- immediate
  _ -> u

stValue :: Model -> Value -> String
stValue m v = case v of
    Ints is -> case IntSet.toList is of
      [i] -> rb $ toStr i m
      l -> '{' : unwords (map rb . Set.toList . Set.fromList
        $ map (`toStr` m) l) ++ "}"
    VSet vs -> '{' : unwords (Set.toList $ Set.map (stValue m) vs) ++ "}"

-- | replace space
rb :: String -> String
rb = map ( \ c -> if c == ' ' then '_' else c)

toStr :: Int -> Model -> String
toStr i = IntMap.findWithDefault (error $ "toStr: " ++ show i) i . intMap

toInts :: Model -> [String] -> Value
toInts m = Ints . IntSet.fromList . map (toInt m)

toInt :: Model -> String -> Int
toInt m v = Map.findWithDefault (error $ "toInt: " ++ v) v $ strMap m

strToBase :: Model -> String -> [Base]
strToBase m v = let t (e, f, b) = if e v `Set.member` f m then (b :) else id
  in t (Role, roles, R)
  . t (Name, users, U)
  . t (Operation, operations, OP)
  . t (Resource, objects, OBJ)
  . t (id, Map.keysSet . sessions, S)
  $ t (id, Set.map pStr . permissions, P) []

illegalActiveRoles :: Model -> S -> Set.Set R
illegalActiveRoles m s =
  activeRoles s Set.\\ rolesOfR (rh m) (rolesOfU m $ user s)

rhCycle :: Map.Map R (Set.Set R) -> R -> Set.Set R
rhCycle m k = let rs = juniors m Set.empty k in
  if k `Set.member` rs then rs else Set.empty
