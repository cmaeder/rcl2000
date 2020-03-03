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
  let m = initModel . foldl readSets (foldr readS (foldr readRH (foldr readPA
        (foldr readUA emptyModel $ lines uas) $ lines pas) $ lines rhs)
        $ lines ses) $ lines ts
  return $ assert "read" (properStructure m) m

readUA :: String -> Model -> Model
readUA s m = case words s of
  u : rs -> foldr (addUA u) (addU u m) rs
  _ -> m

readPA :: String -> Model -> Model
readPA s m = case words s of
  oP : oBj : rs -> foldr (addPA oP oBj) (addP oP oBj m) rs
  _ -> m

readRH :: String -> Model -> Model
readRH s m = case words s of
  r : rs -> addRH r rs m
  _ -> m

readS :: String -> Model -> Model
readS s m = case words s of
  i : u : rs -> addSURs i u rs m
  _ -> m

addSURs :: String -> String -> [String] -> Model -> Model
addSURs s u rs m = addS s (addU u $ foldr addR m rs)
  { sessions = Map.insert s (Session (Name u) . Set.fromList $ map Role rs)
    $ sessions m }

-- user and role
addUA :: String -> String -> Model -> Model
addUA u r m = addR r m { ua = Set.insert (Name u, Role r) $ ua m }

addPA :: String -> String -> String -> Model -> Model
addPA oP oBj r m = addR r m { pa = Set.insert (strP oP oBj, Role r) $ pa m }

addRH :: String -> [String] -> Model -> Model
addRH r js m = addR r (foldr addR m js)
    { rh = Map.insert (Role r) (Set.fromList $ map Role js) $ rh m }

readSets :: Model -> String -> Model
readSets m s = case words s of
  n : vs@(_ : _) ->
    let mt : ts = map (findSetType m) vs
        us = userSets m
        r = addS n m
    in case mt of
      Just t -> if all (== mt) ts then r
        { userSets = Map.insert n (Set t, joinValues m t vs) us }
        else case (t, ts) of
          (ElemTy OP, Just (ElemTy OBJ) : _) -> r
            { userSets = Map.insert n
              (Set $ ElemTy P, psToValue m $ joinPs m vs) us }
          _ -> error $ "readSets1: " ++ s
      _ -> error $ "readSets2: " ++ s
  _ -> m

joinValues :: Model -> SetType -> [String] -> Value
joinValues m t vs = case t of
  ElemTy _ -> toInts m vs
  _ -> VSet . Set.fromList $ map
    (snd . flip (Map.findWithDefault $ error "joinValues") (userSets m)) vs

joinPs :: Model -> [String] -> [P]
joinPs m l = case l of
  oP : oBj : r -> (if Set.member (Operation oP) (operations m)
    && Set.member (Object oBj) (objects m) then (strP oP oBj :) else id)
    $ joinPs m r
  _ -> []

psToValue :: Model -> [P] -> Value
psToValue m = toInts m . map pStr

findSetType :: Model -> String -> Maybe SetType
findSetType m v = case Map.lookup v $ userSets m of
  Just (t, _) -> Just t
  Nothing -> case strToBase m v of
    b : _ -> Just $ ElemTy b
    [] -> Nothing