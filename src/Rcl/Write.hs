module Rcl.Write (writePU, writeS) where

import Control.Exception (IOException, handle)
import Control.Monad (unless, when)
import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Data

import System.Directory (doesFileExist)

-- | write file content with output depending on verbosity
writeMyFile :: Int -> FilePath -> String -> IO ()
writeMyFile v f s = handle (\ e -> do
  print (e :: IOException)
  return ()) $ do
    b <- doesFileExist f
    when (v > 0) . putStrLn $ (if b then "over" else "") ++ "writing: " ++ f
    writeFile f s
    when (v > 1) . putStrLn $ "successfully written: " ++ f
    unless (any isAlphaNum s) . putStrLn $ "WARN: no text in: " ++ f

writeWordsFile :: Int -> FilePath -> [[String]] -> IO ()
writeWordsFile v f = writeMyFile v f . unlines . map unwords

getS :: Model -> [[String]]
getS = map
  (\ (s, v) -> s : name (user v) : map role (Set.toList $ activeRoles v))
  . Map.toList . sessions

writeS :: Int -> FilePath -> Model -> IO ()
writeS v f = writeWordsFile v f . getS

getPU :: Model -> [[String]]
getPU = map
  (\ (p, s) -> operation (op p) : resource (obj p) : map name (Set.toList s))
  . Map.toList . Map.fromListWith Set.union
  . map ( \ (u, p) -> (p, Set.singleton u)) . Set.toList . up

writePU :: Int -> FilePath -> Model -> IO ()
writePU v f = writeWordsFile v f . getPU
