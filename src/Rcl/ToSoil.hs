module Rcl.ToSoil (toSoil) where

import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast (Base (..), SetType (..), baseType, foldSetType)
import Rcl.Data
import Rcl.ToOcl (aggName, enc, tr)

toSoil :: Model -> String
toSoil m = unlines $ let
  ps = permissions m
  pl = Set.toList ps
  ss = sessions m
  sl = Map.toList ss
  us = userSets m
  rs = roles m
  hs = rhim m
  ks = Set.union (Map.keysSet hs) . Map.keysSet $ invim m
  in new R role (Set.difference rs ks)
  ++ map (mkNew "HR" . codeB R . role) (Set.toList ks)
  ++ new U name (users m)
  ++ new OP operation (operations m)
  ++ new OBJ resource (objects m)
  ++ new P pStr ps
  ++ set "op" (codeB P . pStr) (codeB OP . operation . op) pl
  ++ set "obj" (codeB P . pStr) (codeB OBJ . resource . obj) pl
  ++ new S id (Map.keysSet ss)
  ++ set "user" (codeB S . fst) (codeB U . name . user . snd) sl
  ++ insert "UA" (codeB U . name) role (Set.toList $ ua m)
  ++ insert "PA" (codeB P . pStr) role (Set.toList $ pa m)
  ++ insert "RH" (codeB R . role) role (concatMap (\ (r, l)
    -> map (\ e -> (r, e)) $ Set.toList l) $ Map.toList hs)
  ++ insert "SessionRoles" (codeB S) role
    (concatMap (\ (i, s) -> map (\ e -> (i, e))
    . Set.toList $ activeRoles s) sl)
  ++ map (\ s -> mkNew (tr s) $ tr s) ("RBAC" : Map.keys us)
  ++ map (\ (s, t, r) -> mkInsert (tr s) (code t r) $ aggName t) (concatMap
     (\ (s, (t, _, l)) -> map (\ e -> (s, t, e)) l) $ Map.toList us)

mkNew :: String -> String -> String
mkNew c n = "!create " ++ n ++ " : " ++ c

new :: Base -> (a -> String) -> Set.Set a -> [String]
new b f = map (mkNew (show b) . codeB b . f) . Set.toList

set :: String -> (a -> String) -> (a -> String) -> [a] -> [String]
set a f g = map $ \ o -> '!' : f o ++ "." ++ a ++ " := " ++ g o

mkInsert :: String -> String -> String -> String
mkInsert a b r = "!insert (" ++ a ++ "," ++ b ++ ") into " ++ r

insert :: String -> (a -> String) -> (b -> String) -> [(a, b)] -> [String]
insert r f g = map $ \ (a, b) -> mkInsert (f a) (codeB R $ g b) r

depth :: SetType -> Int
depth = foldSetType (+ 1) $ const 0

baseSet :: SetType -> Maybe Base
baseSet s = if depth s <= 1 then Just $ baseType s else Nothing

code :: SetType -> String -> String
code = maybe tr codeB . baseSet

codeB :: Base -> String -> String
codeB b s = case words s of
  [a, o] -> "p_" ++ enc a ++ "_o_" ++ enc o
  _ -> map toLower (show b) ++ '_' : enc s
