module Rcl.Read (readModel, readTypes, readMyFile) where

import Control.Exception (handle, IOException)
import Control.Monad (foldM, when, unless)
import Data.Char (isLetter, isAlphaNum)
import Data.List (find, partition, stripPrefix)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Check (properStructure)
import Rcl.Data
import Rcl.Model (addS, addU, checkU, addP, addR, addSURs, initRH)

import System.Directory (doesFileExist)

readMyFile :: FilePath -> IO String
readMyFile f = handle (\ e -> do
  print (e :: IOException)
  return "") $ do
    b <- doesFileExist f
    if b then readFile f else do
      putStrLn $ "missing file: " ++ f
      return ""

readWordsFile :: FilePath -> IO [[String]]
readWordsFile f = do
  s <- readMyFile f
  let l = map words $ lines s
  checkWords f . zip l $ [(1 :: Int) ..]
  return l

isRcl :: String -> Bool
isRcl s = case s of
  f : r -> isLetter f && all (\ c -> isAlphaNum c || c == '_') r
  _ -> False

checkWords :: FilePath -> [([String], Int)] -> IO ()
checkWords f = mapM_ $ \ (ws, i) -> do
    let e = filter (not . isRcl) ws
    unless (null e) . putStrLn $ "WARN: " ++ f ++ ':' : show i
      ++ ": no legal RCL name" ++ case e of
        [w] -> ": " ++ w
        _ -> "s: " ++ unwords e

readTypes :: FilePath -> IO UserTypes
readTypes f =
  readWordsFile f >>= foldM readType Map.empty

readType :: UserTypes -> [String] -> IO UserTypes
readType u s = case s of
  r : l -> case strT r of
    Just t -> if null l then do
      putStrLn $ "missing names for: " ++ r
      return u
      else return $ foldr (`Map.insert` t) u l
    Nothing -> do
      putStrLn $ "illegal type: " ++ r
      putStrLn $ "ignoring names: " ++ unwords l
      return u
  [] -> return u

strT :: String -> Maybe SetType
strT s = case stripPrefix "SetOf" s of
  Just r -> SetOf <$> strT r
  Nothing -> ElemTy <$> find ((== s) . show) primTypes

readModel :: [FilePath] -> IO Model
readModel l = case l of
  [rhf, uaf, paf, sf, uf] -> do
    m1 <- readWordsFile rhf >>= foldM readRH emptyModel
    m2 <- readWordsFile uaf >>= foldM readUA (initRH m1)
    m3 <- readWordsFile paf >>= foldM readPA m2
    m4 <- readWordsFile sf >>= foldM readS m3
    m5 <- readWordsFile uf >>= foldM readSets m4
    if properStructure m5 then return m5 else do
      putStrLn "internal model error after readModel"
      putStrLn "please report this and include your input files"
      return m5
  _ -> error "readModel"

readUA :: Model -> [String] -> IO Model
readUA m s = case s of
  u : rs -> do
    when (checkU u m) . putStrLn $ "user already known: " ++ u
    when (null rs) . putStrLn $ "no roles assigned to user: " ++ u
    return $ foldr (addUA u) (addU u m) rs
  _ -> return m

readPA :: Model -> [String] -> IO Model
readPA m s = case s of
  oP : oBj : rs -> do
    let ps = permissions m
        p = strP oP oBj
        sp = pStr p
        p_ = pStr_ p
    if p `Set.member` ps then putStrLn $ "permission already known: " ++ sp
      else when (p_ `elem` map pStr_ (Set.toList ps))
      . putStrLn $ "WARN: overlapping permission representation: " ++ p_
    when (null rs) . putStrLn $ "no roles assigned to permission: " ++ sp
    return $ foldr (addPA $ strP oP oBj) (addP oP oBj m) rs
  [p] -> do
    putStrLn $ "provide two words op and obj for (ignored) permission: " ++ p
    return m
  _ -> return m

readRH :: Model -> [String] -> IO Model
readRH m s = case s of
  r : rs -> addRH r rs m
  _ -> return m

readS :: Model -> [String] -> IO Model
readS m s = case s of
  i : u : rs -> case addSURs i u rs m of
    Left e -> do
      putStrLn e
      putStrLn $ "ignoring session: " ++ i
      return m
    Right n -> do
      when (null rs) . putStrLn $ "no roles activated in session: " ++ i
      return n
  _ -> return m

-- user and role
addUA :: String -> String -> Model -> Model
addUA u r m = addR r m { ua = Set.insert (Name u, Role r) $ ua m }

addPA :: P -> String -> Model -> Model
addPA p r m =
  addR r m { pa = Set.insert (p, Role r) $ pa m }

addRH :: String -> [String] -> Model -> IO Model
addRH r js m = let
  m1 = addR r $ foldr addR m js
  k = Role r
  in if null js then do
    when (k `Set.member` roles m) . putStrLn $ "role already known: " ++ r
    return m1
  else do
    let rh0 = rh m
        v = Map.insertWith Set.union k (Set.fromList $ map Role js) rh0
        c = rhCycle v k
        cs = map role $ Set.toList c
    when (k `Map.member` rh0) . putStrLn $ "senior role already known: " ++ r
    if Set.null c then return m1 { rh = v } else do
      putStrLn $ "cyclic role: " ++ unwords (r : cs)
      putStrLn $ "ignoring hierarchy of: " ++ unwords (r : js)
      return m1

readSets :: Model -> [String] -> IO Model
readSets m s = case s of
  n : vs@(_ : _) -> let
    mts = mapM (findSetType m) vs
    us = userSets m
    r = addS n m
    ign = putStrLn $ "ignoring user set: " ++ n
    in if Map.member n us || n `elem` map show primTypes then do
      putStrLn $ "known user set: " ++ n
      ign
      return m
    else case mts of
      Just ts@(t : rs) | all (== t) rs -> return r
        { userSets = Map.insert n (SetOf t, joinValues m t vs, vs) us }
        | allPs ts -> do
        let os = joinPs m vs
            (cs, es) = partition (`Set.member` permissions m) os
        unless (null es) . putStrLn $ "ignoring unknown permissions: "
          ++ unwords (map pStr es)
        return r
          { userSets = let ps = map pStr cs in
            Map.insert n (SetOf $ ElemTy P, toInts m ps, ps) us }
      _ -> do
        putStrLn $ "unknown or inhomogeneous elements: " ++ unwords vs
        ign
        return m
  _ -> return m

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
  oP : oBj : r -> strP oP oBj : joinPs m r
  _ -> []

findSetType :: Model -> String -> Maybe SetType
findSetType m v = case Map.lookup v $ userSets m of
  Just (t, _, _) -> Just t
  Nothing -> case strToBase m v of
    b : _ -> Just $ ElemTy b
    [] -> Nothing
