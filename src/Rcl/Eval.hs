module Rcl.Eval (evalInput, getAllUserTypes) where

import Control.Monad (unless)
import Data.Char
import qualified Data.IntMap as IntMap (empty)
import qualified Data.Map as Map (delete, insertWith, member, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (singleton, union)

import Rcl.Ast
import Rcl.Check (checkAccess)
import Rcl.Data
import Rcl.Interpret (eval, interprets)
import Rcl.Model (addSURs, initSess)
import Rcl.Parse (pLet, set)
import Rcl.Print (lineStmt)
import Rcl.Type (typeSet, wellTyped)

import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryRemovingAllDupes)
import System.IO (hSetEncoding, stdin, utf8)
import Text.ParserCombinators.Parsec

evalInput :: [Let] -> Model -> IO ()
evalInput l m = let ls = map lineStmt l in do
  hSetEncoding stdin utf8
  prefs <- readPrefs ".haskeline"
  runInputTWithPrefs prefs
    defaultSettings { historyFile = Just ".haskeline_history" }
    $ modifyHistory (flip (foldl $ flip addHistoryRemovingAllDupes) ls)
    >> loop l m

getAllUserTypes :: Model -> UserTypes
getAllUserTypes m =
  let ins f b a = Map.insertWith Set.union (f a) . Set.singleton $ ElemTy b
  in foldr (ins resource OBJ) (foldr (ins fst S) (foldr (ins pStr_ P)
  (foldr (ins name U) (foldr (ins role R) (getUserTypes m) $ roles m)
  $ users m) $ permissions m) . Map.toList $ sessions m) $ objects m

loop :: [Let] -> Model -> InputT IO ()
loop l m = do
  i <- getInputLine "rcl2000> "
  outputStrLn $ fromMaybe "" i
  case i of
    Nothing -> return () -- Ctrl-D pressed
    Just s -> let
      us = getAllUserTypes m
      fp = parseAndType us pLet wellTyped s
      sp = parseAndType us set typeSet s
      ck w k = case sp of
        Right (_, Just _) -> False
        _ -> ckCmd w k
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
        (Right (e, Just a), _) -> do
          unless (null e) $ mapM_ outputStrLn e
          outputStrLn $ case eval m IntMap.empty a of
            Left f -> f
            Right v -> stValue m v
          loop l m
        (_, Right (e, Just f)) -> do
          unless (null e) $ mapM_ outputStrLn e
          outputStr (interprets us m [f])
          loop l m
        (Left err1, Left err2) -> do
          outputStrLn err1
          outputStrLn err2
          loop l m
        (Right (err1, _), _) -> do
          mapM_ outputStrLn err1
          loop l m
        (_, Right (err2, _)) -> do
          mapM_ outputStrLn err2
          loop l m

ckCmd :: String -> String -> Bool
ckCmd w k = map toLower w `elem` keys k

ckAd :: String -> Bool
ckAd s = map toLower s `elem` ["add", "del", "delete"]

isAdd :: String -> Bool
isAdd s = map toLower s == "add"

keys :: String -> [String]
keys k = let l s = [':' : s, s] in l (take 1 k) ++ l k

parseAndType :: UserTypes -> Parser a -> (UserTypes -> a -> ([String], Maybe a))
   -> String -> Either String ([String], Maybe a)
parseAndType us p tc s = case parse (spaces *> p <* eof) "" s of
  Right a -> Right $ tc us a
  Left err -> Left $ show err

printHelpText :: InputT IO ()
printHelpText =
  outputStrLn $ unlines
  [ "enter set or statement or any of the following commands."
  , "commands can abbreviated or preceeded with a colon, i.e. ':q':"
  , "help, quit, or exit"
  , "access <sessionId> <operation> <object>"
  , "session add|del <sessionId> [<user> <roles>*]"]
