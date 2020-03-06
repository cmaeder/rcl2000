{-# LANGUAGE TupleSections #-}
module Rcl.ToSoil where

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Data

toSoil :: Model -> [String]
toSoil m = let
  ps = permissions m
  pl = Set.toList ps
  ss = sessions m
  sl = Map.toList ss
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
    (Map.keys $ userSets m)

-- | sanitize names for use
cv :: String -> String
cv = filter (\ c -> isAscii c && (isAlphaNum c || c == '_')) .
  map (\ c -> if c `elem` "- " then '_' else c)

new :: String -> (a -> String) -> Set.Set a -> [String]
new b f =
  map (\ a -> "!new " ++ b ++ "('" ++ cv (f a) ++ "')") . Set.toList
