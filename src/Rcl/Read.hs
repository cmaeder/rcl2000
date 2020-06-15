module Rcl.Read (readModel, readTypes, readMyFile) where

import Control.Exception (IOException, handle)
import Control.Monad (foldM, unless, when)
import Data.Char (isAlphaNum, isLetter)
import Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Check (properStructure)
import Rcl.Data
import Rcl.Model (addP, addR, addS, addSURs, addU, checkU, initRH, insSet)
import Rcl.Parse (pType, setDef)
import Rcl.Type (isElem)

import System.Directory (doesFileExist, makeAbsolute)
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, utf8)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

readMyFile :: Int -> FilePath -> IO String
readMyFile v f = handle (\ e -> do
  print (e :: IOException)
  return "") $ do
    b <- doesFileExist f
    if b then do
        when (v > 0) . putStrLn $ "reading: " ++ f
        h <- openFile f ReadMode
        hSetEncoding h utf8
        s <- hGetContents h
        when (v > 1) $ do
          a <- makeAbsolute f
          putStrLn $ "successfully read: " ++ a
        unless (any isAlphaNum s) . putStrLn
          $ "WARN: no text in: " ++ f
        return s
      else do
        putStrLn $ "missing file: " ++ f
        return ""

readWordsFile :: Int -> FilePath -> IO [[String]]
readWordsFile v f = do
  s <- readMyFile v f
  let l = map words $ lines s
  checkWords f . zip l $ [(1 :: Int) ..]
  return l

readSetsFile :: Int -> FilePath -> IO [(Int, String)]
readSetsFile v = fmap (zip [(1 :: Int) ..] . lines) . readMyFile v

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

readTypes :: Int -> FilePath -> IO UserTypes
readTypes v f =
  readWordsFile v f >>= foldM (readType f) builtinTypes . zip [(1 :: Int) ..]

lp :: Int -> String
lp i = " (line " ++ show i ++ "): "

readType :: FilePath -> UserTypes -> (Int, [String]) -> IO UserTypes
readType f u (i, s) = let p = lp i in case s of
  r : l -> case parse (pType <* eof) f r of
    Right t -> if null l then do
        putStrLn $ "missing names for" ++ p ++ r
        return u
      else
        foldM (\ m n -> case Map.lookup n m of
          Nothing -> return $ Map.insert n (Set.singleton t) m
          Just e -> if Set.member t e then do
              putStrLn $ "set already known" ++ p ++ n ++ ":" ++ r
              return m
            else return $ Map.insert n (Set.insert t e) m) u l
    Left e -> do
      putStrLn $ "illegal type" ++ p ++ r
      print $ setErrorPos (setSourceLine (errorPos e) i) e
      putStrLn $ "ignoring names" ++ p ++ unwords l
      return u
  [] -> return u

readModel :: Int -> [FilePath] -> IO Model
readModel v l = case l of
  [rhf, uaf, paf, sf, uf] -> do
    m1 <- readWordsFile v rhf >>= foldM readRH emptyModel
    m2 <- readWordsFile v uaf >>= foldM readUA (initRH m1)
    m3 <- readWordsFile v paf >>= foldM readPA m2
    m4 <- readWordsFile v sf >>= foldM readS m3
    m5 <- readSetsFile v uf >>= foldM (readSets uf) m4
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

readSets :: FilePath -> Model -> (Int, String) -> IO Model
readSets f m (i, s) = case parse setDef f s of
  Left e -> do
      print $ setErrorPos (setSourceLine (errorPos e) i) e
      putStrLn $ "ignoring line: " ++ s
      return m
  Right ((n, mt) : ws@(_ : _)) -> let
    ts = map (findSetType m) ws
    vs = map fst ws
    is = case mt of
        Just (SetOf t) -> if all (Set.member t) ts then Set.singleton t else
          Set.empty
        _ -> foldr1 Set.intersection ts
    js = Set.filter (not . isElem) is
    ks = if Set.size js > 0 then js else is
    r = addS n m
    p = lp i
    ign = putStrLn $ "ignoring user set" ++ p ++ n
    kn t = do
      putStrLn $ "known user set" ++ p ++ n ++ ":" ++ stSet t
      ign
      return m
    in case Set.toList ks of
      [t] -> let st = SetOf t in
        if knownSet n st m then kn st else
        return $ insSet n st (joinValues m t vs) vs r
      [] | allPs ts && maybe True (== toSet P) mt -> do
        let os = joinPs m vs
            (cs, es) = partition (`Set.member` permissions m) os
            ps = map pStr cs
            pt = toSet P
        unless (null es) . putStrLn $ "ignoring unknown permissions" ++ p
          ++ unwords (map pStr es)
        if knownSet n pt m then kn pt else
          return $ insSet n pt (toInts m ps) ps r
      _ -> do
        putStrLn $ "unknown, ambiguous or inhomogeneous elements" ++ p ++ s
        ign
        return m
  _ -> return m

knownSet :: String -> SetType -> Model -> Bool
knownSet s t m = Map.member t (Map.findWithDefault Map.empty s $ userSets m)
  || any (\ (b, r) -> r == s && t == toSet b) primTypes

allPs :: [Set.Set SetType] -> Bool
allPs l = case l of
  (f : s : r) ->
    ElemTy OP `Set.member` f && ElemTy OBJ `Set.member` s && allPs r
  [] -> True
  _ -> False

joinValues :: Model -> SetType -> [String] -> Value
joinValues m t vs = case t of
  ElemTy _ -> toInts m vs
  _ -> VSet . Set.fromList $ map (\ s -> getValue s t m) vs

getValue :: String -> SetType -> Model -> Value
getValue s t = fst . Map.findWithDefault (error "getValue") t
  . Map.findWithDefault Map.empty s . userSets

joinPs :: Model -> [String] -> [P]
joinPs m l = case l of
  oP : oBj : r -> strP oP oBj : joinPs m r
  _ -> []

findSetType :: Model -> (String, Maybe SetType) -> Set.Set SetType
findSetType m (v, mt) = let
  ts = Set.union (Map.keysSet . Map.findWithDefault Map.empty v $ userSets m)
    . Set.fromList . map ElemTy $ strToBase m v in case mt of
  Nothing -> ts
  Just t -> if Set.member t ts then Set.singleton t else Set.empty
