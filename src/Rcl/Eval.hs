module Rcl.Eval where

import Control.Monad (unless)
import Data.Char (isSpace)
import qualified Data.IntMap as IntMap (empty)

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
        case parse (set <* eof) "" $ dropWhile isSpace s of
          Right a -> case typeOfSet (getUserTypes m) a of
            Just _ -> outputStrLn . stValue m $ eval m IntMap.empty a
            Nothing -> outputStrLn $ "wongly typed set: " ++ ppSet a
          Left err -> outputStrLn $ show err
        loop [] m
    _ -> loop [] m
