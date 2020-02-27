module Rcl.Read (readModel) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Model

readModel :: IO Model
readModel = do
  uas <- readFile "examples/ua.txt"
  pas <- readFile "examples/pa.txt"
  rhs <- readFile "examples/rh.txt"
  ses <- readFile "examples/s.txt"
  ts <- readFile "examples/sets.txt"
  return . initModel . foldr readSets (foldr readS (foldr readRH (foldr readPA
    (foldr readUA emptyModel $ lines uas) $ lines pas) $ lines rhs)
    $ lines ses) $ lines ts

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

readSets :: String -> Model -> Model
readSets s m = let us = userSets m in case words s of
  n : vs@(_ : _) -> let mt : ts = map (findSetType m) vs in
    case mt of
      Just t -> if all (== mt) ts then m
        { userSets = Map.insert n (Set t, joinValues m t vs) us }
        else case (t, ts) of
          (ElemTy OP, Just (ElemTy OBJ) : _) -> m
            { userSets = Map.insert n
              (Set $ ElemTy P, psToValue m $ joinPs m vs) us }
          _ -> m
      _ -> m
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
  Nothing
    | Set.member (Role v) $ roles m -> Just $ ElemTy R
    | Set.member (Name v) $ users m -> Just $ ElemTy U
    | Set.member (Operation v) $ operations m -> Just $ ElemTy OP
    | Set.member (Object v) $ objects m -> Just $ ElemTy OBJ
  _ -> Nothing
