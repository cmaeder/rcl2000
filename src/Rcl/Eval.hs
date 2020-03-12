module Rcl.Eval (evalInput) where

import Data.Either (isLeft)
import qualified Data.IntMap as IntMap (empty)
import qualified Data.Map as Map (insert, toList)

import Rcl.Ast
import Rcl.Data
import Rcl.Interpret (eval, interprets)
import Rcl.Parse (set, parser)
import Rcl.Print (ppSet, ppStmt)
import Rcl.Type (typeOfSet, typeErrors)

import System.Console.Haskeline
import System.Console.Haskeline.History
import Text.ParserCombinators.Parsec

evalInput :: [Stmt] -> Model -> IO ()
evalInput l m = let ls = map ppStmt l in
  runInputT defaultSettings $
    modifyHistory (flip (foldr addHistory) ls) >> loop l m

getAllUserTypes :: Model -> UserTypes
getAllUserTypes m =
  foldr (\ o -> Map.insert (object o) $ ElemTy OBJ)
  (foldr (\ p -> Map.insert (fst p) $ ElemTy S)
  (foldr (\ p -> Map.insert (pStr p) $ ElemTy P)
  (foldr (\ u -> Map.insert (name u) $ ElemTy U)
  (foldr (\ r -> Map.insert (role r) $ ElemTy R) (getUserTypes m) $ roles m)
  $ users m) $ permissions m) . Map.toList $ sessions m) $ objects m

loop :: [Stmt] -> Model -> InputT IO ()
loop l m = do
  i <- getInputLine "rcl2000> "
  case i of
    Nothing -> return () -- Ctrl-D pressed
    Just s -> let
      us = getAllUserTypes m
      fp = parseAndType us parser typeErrors s
      sp = parseAndType us (spaces *> set <* eof) typeSet s
      in case words s of
      [] -> loop l m
      [w] | w `elem` [":q", "q", "quit", "exit"] && isLeft sp -> return ()
        | w `elem` [":h", "h", "help"] && isLeft sp -> printHelpText >> loop l m
      _ -> case (sp, fp) of
        (Right a, _) -> outputStrLn (stValue m $ eval us m IntMap.empty a)
          >> loop l m
        (_, Right f) -> outputStrLn (interprets us m f)
          >> loop l m
        (Left err1, Left err2) -> do
          outputStrLn err1
          outputStrLn err2
          loop l m

typeSet :: UserTypes -> Set -> String
typeSet us a = maybe ("wrongly typed set: " ++ ppSet a) (const "")
  $ typeOfSet us a

parseAndType :: UserTypes -> Parser a -> (UserTypes -> a -> String)
   -> String -> Either String a
parseAndType us p tc s = case parse p "" s of
  Right a -> let t = tc us a in
    if null t then Right a else Left t
  Left err -> Left $ show err

printHelpText :: InputT IO ()
printHelpText =
  outputStrLn $ unlines
  ["enter set or statement or any of the following commands"
  , ":h, help, :q, quit, or exit"]
