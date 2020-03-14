module Rcl.Eval (evalInput) where

import Data.Char
import Data.Either (isLeft)
import qualified Data.IntMap as IntMap (empty)
import qualified Data.Map as Map (insert, toList)

import Rcl.Ast
import Rcl.Check (checkAccess)
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
      ck w k = isLeft sp && ckCmd w k
      in case words s of
      [] -> loop l m
      [w] | ck w "exit" -> return ()
        | ck w "quit" -> return ()
        | ck w "help" -> printHelpText >> loop l m
      [w, si, oP, oBj] | ck w "access" -> do
         let ls = checkAccess m si (Operation oP) $ Object oBj
         outputStr (if null ls then "access granted\n" else
                        unlines $ "access denied" : ls)
         loop l m
      _ -> case (sp, fp) of
        (Right a, _) -> do
          outputStrLn $ case eval us m IntMap.empty a of
            Left f -> f
            Right v -> stValue m v
          loop l m
        (_, Right f) -> outputStr (interprets us m f)
          >> loop l m
        (Left err1, Left err2) -> do
          outputStrLn err1
          outputStrLn err2
          loop l m

ckCmd :: String -> String -> Bool
ckCmd w k = map toLower w `elem` keys k

keys :: String -> [String]
keys k = let l s = [':' : s, s] in l (take 1 k) ++ l k

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
  ["enter set or statement or any of the following commands."
  , "commands can abbreviated or preceeded with a colon, i.e. ':q':"
  , "help, quit, or exit"
  , "access <sessionId> <operation> <object>" ]
{-
  , "session add|del <sessionId> [<user> <roles>*]"
  , "role add|del <role> <juniorRole>*"
  , "user add|del <user> <roles>*"
  , "permission add <operation> <object> <roles>*"
  , "permission del <operation>_<object>"
  , "value add|del <name> <name>+" ]
-}
