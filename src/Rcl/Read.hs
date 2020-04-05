{-# LANGUAGE ScopedTypeVariables #-}
module Rcl.Read (readModel, readTypes, addSURs, readMyFile) where


import Control.Exception (handle, IOException)
import Data.List (find, stripPrefix)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Check (properStructure)
import Rcl.Data
import Rcl.Model (addS, addU, checkU, addP, addR, initRH)

import System.Directory (doesFileExist)

readMyFile :: FilePath -> IO String
readMyFile f = handle (\ (e :: IOException) -> do
  print e
  return "") $ do
  b <- doesFileExist f
  if b then readFile f else do
    putStrLn $ "missing file: " ++ f
    return ""

readTypes :: IO UserTypes
readTypes = do
  ts <- readMyFile "examples/types.txt"
  return . foldl readType Map.empty $ lines ts

readType :: UserTypes -> String -> UserTypes
readType u s = case words s of
  r : l -> case strT r of
    Just t -> if null l then error $ "missing names for: " ++ r
      else foldr (`Map.insert` t) u l
    Nothing -> error $ "illegal type: " ++ r
  [] -> u

strT :: String -> Maybe SetType
strT s = case stripPrefix "SetOf" s of
  Just r -> SetOf <$> strT r
  Nothing -> ElemTy <$> find ((== s) . show) primTypes

readModel :: IO Model
readModel = do
  rhs <- readMyFile "examples/rh.txt"
  uas <- readMyFile "examples/ua.txt"
  pas <- readMyFile "examples/pa.txt"
  ses <- readMyFile "examples/s.txt"
  ts <- readMyFile "examples/sets.txt"
  let m = foldl readSets (foldl readS (foldl readPA (foldl readUA
        (initRH . foldl readRH emptyModel $ lines rhs) $ lines uas) $ lines pas)
        $ lines ses) $ lines ts
  if properStructure m then return m else error "readModel"

readUA :: Model -> String -> Model
readUA m s = case words s of
  u : rs | checkU u m -> error $ "user already known: " ++ s
    | otherwise -> foldr (addUA u) (addU u m) rs
  _ -> m

readPA :: Model -> String -> Model
readPA m s = case words s of
  oP : oBj : rs -> foldr (addPA $ strP oP oBj) (addP oP oBj m) rs
  [p] -> error $ "provide two words op and obj for permission: " ++ p
  _ -> m

readRH :: Model -> String -> Model
readRH m s = case words s of
  r : rs -> addRH r rs m
  _ -> m

readS :: Model -> String -> Model
readS m s = case words s of
  i : u : rs -> case addSURs i u rs m of
    Left e -> error e
    Right n -> n
  _ -> m

addSURs :: String -> String -> [String] -> Model -> Either String Model
addSURs s u rs m
  | checkSid s m = Left $ "session identifier already known: " ++ s
  | not $ checkU u m = Left $ "user unknown: " ++ u
  | otherwise = let
      v = Session (Name u) . Set.fromList $ map Role rs
      is = illegalActiveRoles m v
      in if Set.null is then Right $ addS s (foldr addR m rs)
        { sessions = Map.insert s v $ sessions m }
         else Left $ "unassigned roles for user of session: "
           ++ unwords (s : u : map role (Set.toList is))

checkSid :: String -> Model -> Bool
checkSid s m = s `Map.member` sessions m

-- user and role
addUA :: String -> String -> Model -> Model
addUA u r m = addR r m { ua = Set.insert (Name u, Role r) $ ua m }

addPA :: P -> String -> Model -> Model
addPA p r m =
  addR r m { pa = Set.insert (p, Role r) $ pa m }

addRH :: String -> [String] -> Model -> Model
addRH r js m = addR r $ if null js then m else (foldr addR m js)
  { rh = let
    k = Role r
    v = Map.insertWith Set.union k (Set.fromList $ map Role js) $ rh m
    c = rhCycle v k
    cs = map role $ Set.toList c
    in if Set.null c then v else error $ "cyclic role: " ++ unwords (r : cs) }

readSets :: Model -> String -> Model
readSets m s = case words s of
  n : vs@(_ : _) -> let
    mts = mapM (findSetType m) vs
    us = userSets m
    r = addS n m
    in if Map.member n us then error $ "duplicate user set: " ++ s else
      case mts of
      Just ts@(t : rs) | all (== t) rs -> r
        { userSets = Map.insert n (SetOf t, joinValues m t vs, vs) us }
        | allPs ts -> r
        { userSets = let ps = map pStr $ joinPs m vs in
          Map.insert n (SetOf $ ElemTy P, toInts m ps, ps) us }
      _ -> error $ "readSets: " ++ s
  _ -> m

allPs :: [SetType] -> Bool
allPs l = case l of
  (ElemTy OP : ElemTy OBJ : r) -> allPs r
  [] -> True
  _ -> False

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
