{-# LANGUAGE ScopedTypeVariables #-}
module Rcl.Data where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set ((\\))

import Rcl.Ast
import Rcl.Type (isElem, elemType)

newtype U = Name { name :: String } deriving (Eq, Ord, Show)
newtype R = Role { role :: String } deriving (Eq, Ord, Show)
newtype OP = Operation { operation :: String } deriving (Eq, Ord, Show)
newtype OBJ = Object { object :: String } deriving (Eq, Ord, Show)
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
  , userSets :: Map String (SetType, Value)
  , ua :: Set.Set (U, R)
  , pa :: Set.Set (P, R)
  , rh :: Map R (Set.Set R) -- direct junior roles
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
  , strMap = Map.empty
  , intMap = IntMap.empty
  , fctMap = Map.empty
  , opsMap = Map.empty
  , next = 1 }

pStr :: P -> String
pStr p = unwords [operation $ op p, object $ obj p]

strP :: String -> String -> P
strP u v = Permission (Operation u) $ Object v

rolesOfU :: Model -> U -> Set.Set R
rolesOfU m u = Set.foldr
  (\ (v, r) -> if u == v then Set.insert r else id) Set.empty $ ua m

juniors :: Map R (Set.Set R) -> Set.Set R -> R -> Set.Set R
juniors m visited r = let s = Map.findWithDefault Set.empty r m \\ visited
  in if Set.null s then s else Set.union s
     . Set.unions . map (juniors m $ Set.insert r visited) $ Set.toList s

getStrings :: Model -> Base -> Set.Set String
getStrings m b = case b of
  U -> Set.map name $ users m
  R -> Set.map role $ roles m
  OBJ -> Set.map object $ objects m
  OP -> Set.map operation $ operations m
  S -> Map.keysSet $ sessions m
  P -> Set.map pStr $ permissions m

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
        R -> "r"
        _ -> u
      _ -> u
  _ -> u

stValue :: Model -> Value -> String
stValue m v = '{' : case v of
    Ints is -> unwords . map (`toStr` m) $ IntSet.toList is
    VSet vs -> unwords . map (stValue m) $ Set.toList vs
  ++ "}"

toStr :: Int -> Model -> String
toStr i = IntMap.findWithDefault "" i . intMap

strToBase :: Model -> String -> [Base]
strToBase m v = let t (e, f, b) = if e v `Set.member` f m then (b :) else id
  in t (Role, roles, R)
  . t (Name, users, U)
  . t (Operation, operations, OP)
  . t (Object, objects, OBJ)
  . t (id, Map.keysSet . sessions, S)
  $ (case words v of
    [oP, oBj] -> t (const $ strP oP oBj, permissions, P)
    _ -> id) []
