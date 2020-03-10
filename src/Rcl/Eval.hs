module Rcl.Eval where

import Control.Monad (unless)
import Data.Char (isSpace)
import qualified Data.IntMap as IntMap (empty)
import qualified Data.Map as Map (insert, toList)

import Rcl.Ast
import Rcl.Data
import Rcl.Interpret (eval)
import Rcl.Parse (set)
import Rcl.Print (ppSet, ppStmt)
import Rcl.Type (typeOfSet)

import System.Console.Haskeline
import System.Console.Haskeline.History
import Text.ParserCombinators.Parsec

evalInput :: [Stmt] -> Model -> IO ()
evalInput l = let ls = map ppStmt l in
  runInputT defaultSettings . loop ls

getAllUserTypes :: Model -> UserTypes
getAllUserTypes m =
  foldr (\ o -> Map.insert (object o) $ ElemTy OBJ)
  (foldr (\ p -> Map.insert (fst p) $ ElemTy S)
  (foldr (\ p -> Map.insert (pStr p) $ ElemTy P)
  (foldr (\ u -> Map.insert (name u) $ ElemTy U)
  (foldr (\ r -> Map.insert (role r) $ ElemTy R) (getUserTypes m) $ roles m)
  $ users m) $ permissions m) . Map.toList $ sessions m) $ objects m

loop :: [String] -> Model -> InputT IO ()
loop l m = do
  unless (null l) $ modifyHistory (flip (foldr addHistory) l)
  i <- getInputLine "rcl2000> "
  case i of
    Just s
      | s `elem` ["q", "quit", "exit"] -> return ()
      | s `elem` ["h", "help"] -> do
        outputStrLn "enter set expression"
        loop [] m
      | not $ null s -> do
        let us = getAllUserTypes m
        case parse (set <* eof) "" $ dropWhile isSpace s of
          Right a -> case typeOfSet us a of
            Just _ -> outputStrLn . stValue m $ eval us m IntMap.empty a
            Nothing -> outputStrLn $ "wongly typed set: " ++ ppSet a
          Left err -> outputStrLn $ show err
        loop [] m
    _ -> loop [] m
