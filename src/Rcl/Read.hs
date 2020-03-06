module Rcl.Read (readModel) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Check (properStructure)
import Rcl.Data
import Rcl.Model (initModel, addS, addU, addP, addR, toInts)

readModel :: IO Model
readModel = do
  uas <- readFile "examples/ua.txt"
  pas <- readFile "examples/pa.txt"
  rhs <- readFile "examples/rh.txt"
  ses <- readFile "examples/s.txt"
  ts <- readFile "examples/sets.txt"
  let m = initModel . foldl readSets (foldl readS (foldl readRH (foldl readPA
        (foldl readUA emptyModel $ lines uas) $ lines pas) $ lines rhs)
        $ lines ses) $ lines ts
  return $ assert "read" (properStructure m) m

readUA :: Model -> String -> Model
readUA m s = case words s of
  u : rs -> foldr (addUA u) (addU u m) rs
  _ -> m

readPA :: Model -> String -> Model
readPA m s = case words s of
  oP : oBj : rs -> foldr (addPA oP oBj) (addP oP oBj m) rs
  _ -> m

readRH :: Model -> String -> Model
readRH m s = case words s of
  r : rs -> addRH r rs m
  _ -> m

readS :: Model -> String -> Model
readS m s = case words s of
  i : u : rs -> addSURs i u rs m
  _ -> m

addSURs :: String -> String -> [String] -> Model -> Model
addSURs s u rs m = addS s (addU u $ foldr addR m rs)
  { sessions = let
      v = Session (Name u) . Set.fromList $ map Role rs
      is = illegalActiveRoles m v
      in assert ("invalid session roles: "
        ++ unwords (s : u : map role (Set.toList is)))
      (Set.null is) . Map.insert s v $ sessions m }

-- user and role
addUA :: String -> String -> Model -> Model
addUA u r m = addR r m { ua = Set.insert (Name u, Role r) $ ua m }

addPA :: String -> String -> String -> Model -> Model
addPA oP oBj r m = addR r m { pa = Set.insert (strP oP oBj, Role r) $ pa m }

addRH :: String -> [String] -> Model -> Model
addRH r js m = addR r (foldr addR m js)
    { rh = let
        k = Role r
        v = Map.insert k (Set.fromList $ map Role js) $ rh m
        c = rhCycle v k
        cs = map role $ Set.toList c
      in assert ("cyclic role: " ++ unwords (r : cs)) (Set.null c) v }

readSets :: Model -> String -> Model
readSets m s = case words s of
  n : vs@(_ : _) ->
    let mt : ts = map (findSetType m) vs
        us = userSets m
        r = addS n m
    in case mt of
      Just t -> if all (== mt) ts then r
        { userSets = Map.insert n (Set t, joinValues m t vs, vs) us }
        else case (t, ts) of
          (ElemTy OP, Just (ElemTy OBJ) : _) -> r
            { userSets = let ps = map pStr $ joinPs m vs in
               Map.insert n (Set $ ElemTy P, toInts m ps, ps) us }
          _ -> error $ "readSets1: " ++ s
      _ -> error $ "readSets2: " ++ s
  _ -> m

joinValues :: Model -> SetType -> [String] -> Value
joinValues m t vs = case t of
  ElemTy _ -> toInts m vs
  _ -> VSet . Set.fromList $ map ((\ (_, v, _) -> v)
    . flip (Map.findWithDefault $ error "joinValues") (userSets m)) vs

joinPs :: Model -> [String] -> [P]
joinPs m l = case l of
  oP : oBj : r -> (if Set.member (Operation oP) (operations m)
    && Set.member (Object oBj) (objects m) then (strP oP oBj :) else id)
    $ joinPs m r
  _ -> []

findSetType :: Model -> String -> Maybe SetType
findSetType m v = case Map.lookup v $ userSets m of
  Just (t, _, _) -> Just t
  Nothing -> case strToBase m v of
    b : _ -> Just $ ElemTy b
    [] -> Nothing
