module Rcl.Eval (evalInput) where

import Control.Monad (unless)
import Data.Char
import Data.Either (isLeft)
import qualified Data.IntMap as IntMap (empty)
import qualified Data.Map as Map (delete, insertWith, member, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (singleton, toList, union)

import Rcl.Ast
import Rcl.Check (checkAccess)
import Rcl.Data
import Rcl.Interpret (eval, interprets)
import Rcl.Model (addSURs, initSess)
import Rcl.Parse (parser, set)
import Rcl.Print (lineStmt, ppSet)
import Rcl.Type (typeErrors, typeOfSet)

import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryRemovingAllDupes)
import System.IO (hSetEncoding, stdin, utf8)
import Text.ParserCombinators.Parsec

evalInput :: [Stmt] -> Model -> IO ()
evalInput l m = let ls = map lineStmt l in do
  hSetEncoding stdin utf8
  prefs <- readPrefs ".haskeline"
  runInputTWithPrefs prefs
    defaultSettings { historyFile = Just ".haskeline_history" }
    $ modifyHistory (flip (foldr addHistoryRemovingAllDupes) ls) >> loop l m

getAllUserTypes :: Model -> UserTypes
getAllUserTypes m =
  let ins f b a = Map.insertWith (Set.union) (f a) . Set.singleton $ ElemTy b
  in foldr (ins resource OBJ) (foldr (ins fst S) (foldr (ins pStr_ P)
  (foldr (ins name U) (foldr (ins role R) (getUserTypes m) $ roles m)
  $ users m) $ permissions m) . Map.toList $ sessions m) $ objects m

loop :: [Stmt] -> Model -> InputT IO ()
loop l m = do
  i <- getInputLine "rcl2000> "
  outputStrLn $ fromMaybe "" i
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
         let ls = checkAccess m si oP oBj
         outputStr (if null ls then "access granted\n" else
                        unlines $ "access denied" : ls)
         loop l m
      w : ad : si : urs | ck w "session" && ckAd ad ->
        if isAdd ad then case urs of
          [] -> do
            outputStrLn $ "missing user and roles for session: " ++ si
            loop l m
          u : rs -> case addSURs si u rs m of
            Left e -> outputStrLn e >> loop l m
            Right n -> do
              outputStrLn $ "session added: " ++ si
              loop l $ initSess n
        else let ses = sessions m in if si `Map.member` ses then do
            unless (null urs) . outputStrLn $ "ignoring: " ++ unwords urs
            outputStrLn $ "session deleted: " ++ si
            loop l $ initSess m { sessions = Map.delete si ses }
          else outputStrLn ("unknown session: " ++ si) >> loop l m
      _ -> case (sp, fp) of
        (Right a, _) -> do
          outputStrLn $ case eval us m IntMap.empty a of
            Left f -> f
            Right v -> stValue m v
          loop l m
        (_, Right f) -> outputStr (interprets us m f)
          >> loop l m
        (Left (Left err1), Left (Left err2)) -> do
          outputStrLn err1
          outputStrLn err2
          loop l m
        (Left (Right err1), _) -> do
          outputStrLn err1
          loop l m
        (_, Left (Right err2)) -> do
          outputStrLn err2
          loop l m

ckCmd :: String -> String -> Bool
ckCmd w k = map toLower w `elem` keys k

ckAd :: String -> Bool
ckAd s = map toLower s `elem` ["add", "del", "delete"]

isAdd :: String -> Bool
isAdd s = map toLower s == "add"

keys :: String -> [String]
keys k = let l s = [':' : s, s] in l (take 1 k) ++ l k

typeSet :: UserTypes -> Set -> String
typeSet us a = case Set.toList $ typeOfSet us a of
  [_] -> ""
  _ -> "wrongly typed set: " ++ ppSet a

parseAndType :: UserTypes -> Parser a -> (UserTypes -> a -> String)
   -> String -> Either (Either String String) a
parseAndType us p tc s = case parse p "" s of
  Right a -> let t = tc us a in
    if null t then Right a else Left $ Right t
  Left err -> Left . Left $ show err

printHelpText :: InputT IO ()
printHelpText =
  outputStrLn $ unlines
  [ "enter set or statement or any of the following commands."
  , "commands can abbreviated or preceeded with a colon, i.e. ':q':"
  , "help, quit, or exit"
  , "access <sessionId> <operation> <object>"
  , "session add|del <sessionId> [<user> <roles>*]"]
