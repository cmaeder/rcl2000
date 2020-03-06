{-# LANGUAGE TupleSections #-}
module Rcl.ToSoil (toSoil) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Data
import Rcl.ToOcl (aggName, cv)

toSoil :: Model -> String
toSoil m = unlines $ let
  ps = permissions m
  pl = Set.toList ps
  ss = sessions m
  sl = Map.toList ss
  us = foldr (\ b n -> Map.delete (show b) n) (userSets m) primTypes
  in new "R" role (roles m)
  ++ new "U" name (users m)
  ++ new "OP" operation (operations m)
  ++ new "OBJ" object (objects m)
  ++ new "P" pStr ps
  ++ map (\ p -> '!' : cv (pStr p) ++ ".op := " ++ cv (operation $ op p)) pl
  ++ map (\ p -> '!' : cv (pStr p) ++ ".obj := " ++ cv (object $ obj p)) pl
  ++ new "S" id (Map.keysSet ss)
  ++ map (\ (i, s) -> '!' : cv i ++ ".user := " ++ cv (name $ user s)) sl
  ++ map (\ (u, r) -> "!insert (" ++ cv (name u) ++ "," ++ cv (role r)
          ++ ") into UA") (Set.toList $ ua m)
  ++ map (\ (p, r) -> "!insert (" ++ cv (pStr p) ++ "," ++ cv (role r)
          ++ ") into PA") (Set.toList $ pa m)
  ++ map (\ (s, r) -> "!insert (" ++ cv (role s) ++ "," ++ cv (role r)
          ++ ") into RH")
    (concatMap (\ (r, l) -> map (r,) $ Set.toList l) . Map.toList $ rh m)
  ++ map (\ (s, r) -> "!insert (" ++ cv s ++ "," ++ cv (role r)
          ++ ") into SessionRoles")
    (concatMap (\ (i, s) -> map (i,) . Set.toList $ activeRoles s) sl)
  ++ map (\ s -> "!new " ++ cv s ++ "('" ++ cv s ++ "')")
    (Map.keys us)
  ++ map (\ (s, t, r) -> "!insert (" ++ cv s ++ "," ++ cv r
          ++ ") into " ++ aggName t)
    (concatMap (\ (s, (t, _, l)) -> map (s, t,) l) $ Map.toList us)

new :: String -> (a -> String) -> Set.Set a -> [String]
new b f =
  map (\ a -> "!new " ++ b ++ "('" ++ cv (f a) ++ "')") . Set.toList
