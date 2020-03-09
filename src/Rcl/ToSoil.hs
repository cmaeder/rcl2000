{-# LANGUAGE TupleSections #-}
module Rcl.ToSoil (toSoil) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast (Base (..))
import Rcl.Data
import Rcl.ToOcl (aggName)

toSoil :: Model -> String
toSoil m = unlines $ let
  ps = permissions m
  pl = Set.toList ps
  ss = sessions m
  sl = Map.toList ss
  us = userSets m
  in new R role (roles m)
  ++ new U name (users m)
  ++ new OP operation (operations m)
  ++ new OBJ object (objects m)
  ++ new P pStr ps
  ++ set "op" pStr (operation . op) pl
  ++ set "obj" pStr (object . obj) pl
  ++ new S id (Map.keysSet ss)
  ++ set "user" fst (name . user . snd) sl
  ++ insert "UA" name role (Set.toList $ ua m)
  ++ insert "PA" pStr role (Set.toList $ pa m)
  ++ insert "RH" role role (concatMap (\ (r, l) -> map (r,) $ Set.toList l)
    . Map.toList $ rh m)
  ++ insert "SessionRoles" id role (concatMap (\ (i, s) -> map (i,)
    . Set.toList $ activeRoles s) sl)
  ++ map (\ s -> mkNew s s) ("RBAC" : Map.keys us)
  ++ map (\ (s, t, r) -> mkInsert s r $ aggName t)
    (concatMap (\ (s, (t, _, l)) -> map (s, t,) l) $ Map.toList us)

mkNew :: String -> String -> String
mkNew c n = "!new " ++ c ++ "('" ++ n ++ "')"

new :: Base -> (a -> String) -> Set.Set a -> [String]
new b f = map (mkNew (show b) . f) . Set.toList

set :: String -> (a -> String) -> (a -> String) -> [a] -> [String]
set a f g = map $ \ o -> '!' : f o ++ "." ++ a ++ " := " ++ g o

mkInsert :: String -> String -> String -> String
mkInsert a b r = "!insert (" ++ a ++ "," ++ b ++ ") into " ++ r

insert :: String -> (a -> String) -> (b -> String) -> [(a, b)] -> [String]
insert r f g = map $ \ (a, b) -> mkInsert (f a) (g b) r
