{-# LANGUAGE TupleSections #-}
module Rcl.ToSoil (toSoil) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Data
import Rcl.ToOcl (aggName)

toSoil :: Model -> String
toSoil m = unlines $ let
  ps = permissions m
  pl = Set.toList ps
  ss = sessions m
  sl = Map.toList ss
  us = userSets m
  in new "R" role (roles m)
  ++ new "U" name (users m)
  ++ new "OP" operation (operations m)
  ++ new "OBJ" object (objects m)
  ++ new "P" pStr ps
  ++ map (\ p -> '!' : pStr p ++ ".op := " ++ operation (op p)) pl
  ++ map (\ p -> '!' : pStr p ++ ".obj := " ++ object (obj p)) pl
  ++ new "S" id (Map.keysSet ss)
  ++ map (\ (i, s) -> '!' : i ++ ".user := " ++ name (user s)) sl
  ++ map (\ (u, r) -> "!insert (" ++ name u ++ "," ++ role r
          ++ ") into UA") (Set.toList $ ua m)
  ++ map (\ (p, r) -> "!insert (" ++ pStr p ++ "," ++ role r
          ++ ") into PA") (Set.toList $ pa m)
  ++ map (\ (s, r) -> "!insert (" ++ role s ++ "," ++ role r
          ++ ") into RH")
    (concatMap (\ (r, l) -> map (r,) $ Set.toList l) . Map.toList $ rh m)
  ++ map (\ (s, r) -> "!insert (" ++ s ++ "," ++ role r
          ++ ") into SessionRoles")
    (concatMap (\ (i, s) -> map (i,) . Set.toList $ activeRoles s) sl)
  ++ map (\ s -> "!new " ++ s ++ "('" ++ s ++ "')")
    (Map.keys us)
  ++ map (\ (s, t, r) -> "!insert (" ++ s ++ "," ++ r
          ++ ") into " ++ aggName t)
    (concatMap (\ (s, (t, _, l)) -> map (s, t,) l) $ Map.toList us)
  ++ ["!new RBAC('RBAC')"]

new :: String -> (a -> String) -> Set.Set a -> [String]
new b f =
  map (\ a -> "!new " ++ b ++ "('" ++ f a ++ "')") . Set.toList
