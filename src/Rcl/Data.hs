module Rcl.Data where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set ((\\))

import Rcl.Ast (UserTypes, SetType (..), Base (..), UnOp (..), OptStar (..),
  Ior (..))
import Rcl.Type (isElem, elemType)

newtype U = Name { name :: String } deriving (Eq, Ord, Show)
newtype R = Role { role :: String } deriving (Eq, Ord, Show)
newtype OP = Operation { operation :: String } deriving (Eq, Ord, Show)
newtype OBJ = Resource { resource :: String } deriving (Eq, Ord, Show)
data S = Session { user :: U, activeRoles :: Set.Set R }
  deriving (Eq, Ord, Show)
data P = Permission { op :: OP, obj :: OBJ }
  deriving (Eq, Ord, Show)

data Value = Ints IntSet | VSet (Set.Set Value) deriving (Eq, Ord, Show)

data Model = Model
  { roles :: Set.Set R
  , users :: Set.Set U
  , operations :: Set.Set OP
  , objects :: Set.Set OBJ
  , permissions :: Set.Set P
  , sessions :: Map String S
  , userSets :: Map String (SetType, Value, [String])
  , ua :: Set.Set (U, R)
  , pa :: Set.Set (P, R)
  , rh :: Map R (Set.Set R) -- all junior roles
  , inv :: Map R (Set.Set R) -- inverse senior roles
  , rhim :: Map R (Set.Set R) -- immediate junior roles
  , invim :: Map R (Set.Set R) -- inverse immediate senior roles
  , strMap :: Map String Int
  , intMap :: IntMap String
  , fctMap :: Map String (IntMap IntSet)
  , opsMap :: Map (Int, Int) IntSet -- operations
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
getUserTypes = Map.map (\ (t, _, _) -> t) . userSets

pStrC :: Char -> P -> String
pStrC c p = operation (op p) ++ c : resource (obj p)

pStr :: P -> String
pStr = pStrC ' '

pStr_ :: P -> String
pStr_ = pStrC '_'

strP :: String -> String -> P
strP u v = Permission (Operation u) $ Resource v

usersOfRs :: Model -> Set.Set R -> Set.Set U
usersOfRs m r = Set.foldr
  (\ (u, v) -> if v `Set.member` r then Set.insert u else id) Set.empty $ ua m

usersOfR :: Model -> R -> Set.Set U
usersOfR m = usersOfRs m . rolesOfR (inv m) . Set.singleton

rolesOfR :: Map R (Set.Set R) -> Set.Set R -> Set.Set R
rolesOfR m s = Set.unions $ s
  : map (flip (Map.findWithDefault Set.empty) m) (Set.toList s)

rolesOfU :: Model -> U -> Set.Set R
rolesOfU m u = Set.foldr
  (\ (v, r) -> if u == v then Set.insert r else id) Set.empty $ ua m

rolesOfP :: Model -> P -> Set.Set R
rolesOfP m p = Set.foldr
  (\ (v, r) -> if p == v then Set.insert r else id) Set.empty $ pa m

permissionsOfRs :: Model -> Set.Set R -> Set.Set P
permissionsOfRs m r = Set.foldr
  (\ (p, v) -> if v `Set.member` r then Set.insert p else id) Set.empty $ pa m

permissionsOfR :: Model -> R -> Set.Set P
permissionsOfR m = permissionsOfRs m . rolesOfR (rh m) . Set.singleton

juniors :: Map R (Set.Set R) -> Set.Set R -> R -> Set.Set R
juniors m visited r = let s = Map.findWithDefault Set.empty r m
  in if Set.null s then s else Set.unions . (s :)
  . map (juniors m $ Set.insert r visited) . Set.toList $ s \\ visited

transClosure :: Map R (Set.Set R) -> Map R (Set.Set R)
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
sUnOp t o = let u = take 1 $ show o
  in case o of
  User _ _ -> if t == Just (ElemTy S) then "u" else u
  Object _ -> "B"
  Roles _ -> case t >>= \ s -> if isElem s then Just s else elemType s of
    Just (ElemTy r) -> show r ++ "r"
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
    Ints is -> case IntSet.maxView is of
      Just (i, s) | IntSet.null s -> rb $ toStr i m
      _ -> '{' : unwords (map rb . Set.toList . Set.fromList
        . map (`toStr` m) $ IntSet.toList is) ++ "}"
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
illegalActiveRoles m s = activeRoles s \\ rolesOfR (rh m) (rolesOfU m $ user s)

rhCycle :: Map R (Set.Set R) -> R -> Set.Set R
rhCycle m k = let rs = juniors m Set.empty k in
  if k `Set.member` rs then rs else Set.empty
