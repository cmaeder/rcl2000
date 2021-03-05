module Rcl.Eval (evalInput, getAllUserTypes) where

import Control.Monad (liftM2, unless)
import Control.Monad.Trans (lift)
import Data.Char
import qualified Data.IntMap as IntMap (empty)
import qualified Data.Map as Map (delete, insertWith, member, toList, (!))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (fold, insert, singleton, union)

import Rcl.Ast
import Rcl.Check (checkAccess)
import Rcl.Data
import Rcl.Interpret (eval, interprets)
import Rcl.Model (addSURs, initSess)
import Rcl.Opts (Opts (..), optsFile)
import Rcl.Parse (pLet, set)
import Rcl.Print (lineStmt)
import Rcl.Type (typeSet, wellTyped)
import Rcl.Write (writePU, writeS)

import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryRemovingAllDupes)
import Text.ParserCombinators.Parsec

evalInput :: Opts -> [Let] -> Model -> IO ()
evalInput o l m = let ls = map lineStmt l in do
  prefs <- readPrefs ".haskeline"
  runInputTWithPrefs prefs
    defaultSettings { historyFile = Just ".haskeline_history" }
    $ modifyHistory (flip (foldl $ flip addHistoryRemovingAllDupes) ls)
    >> loop o l m

getAllUserTypes :: Model -> UserTypes
getAllUserTypes m =
  let ins f b a = Map.insertWith Set.union (f a) . Set.singleton $ ElemTy b
  in Set.fold (ins resource OBJ) (foldr (ins fst S) (Set.fold (ins pStr_ P)
  (Set.fold (ins name U) (Set.fold (ins role R) (getUserTypes m) $ roles m)
  $ users m) $ permissions m) . Map.toList $ sessions m) $ objects m

loop :: Opts -> [Let] -> Model -> InputT IO ()
loop o l m = do
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
      [] -> loop o l m
      [w] | ck w "exit" -> return ()
        | ck w "quit" -> return ()
        | ck w "help" -> printHelpText >> loop o l m
      [w, si, oP, oBj] | ck w "access" -> do
         let ls = checkAccess m si oP oBj
         outputStr (if null ls then "access granted\n" else
                        unlines $ "access denied" : ls)
         if null ls then do
             let n = m { up = Set.insert
                       (user $ sessions m Map.! si, strP oP oBj) $ up m }
             lift $ writePU (verbose o) (optsFile o puFile) n
             loop o l n
           else loop o l m
      w : ad : si : urs | ck w "session" && ckAd ad ->
        if isAdd ad then case urs of
          [] -> do
            outputStrLn $ "missing user and roles for session: " ++ si
            loop o l m
          u : rs -> case addSURs si u rs m of
            Left e -> outputStrLn e >> loop o l m
            Right n -> do
              outputStrLn $ "session added: " ++ si
              let nm = initSess n
              lift $ writeS (verbose o) (optsFile o sessFile) nm
              loop o l nm
        else let ses = sessions m in if si `Map.member` ses then do
            unless (null urs) . outputStrLn $ "ignoring: " ++ unwords urs
            outputStrLn $ "session deleted: " ++ si
            let n = initSess m { sessions = Map.delete si ses }
            lift $ writeS (verbose o) (optsFile o sessFile) n
            loop o l n
          else outputStrLn ("unknown session: " ++ si) >> loop o l m
      _ -> case (sp, fp) of
        (Right (e, Just a), _) -> do
          unless (null e) $ mapM_ outputStrLn e
          outputStrLn $ case eval m IntMap.empty a of
            Left f -> f
            Right v -> stValue m v
          loop o l m
        (_, Right (e, Just f)) -> do
          unless (null e) $ mapM_ outputStrLn e
          outputStr (interprets us m [f])
          loop o l m
        (Left err1, Left err2) -> do
          outputStrLn err1
          outputStrLn err2
          loop o l m
        (Right (err1, _), _) -> do
          mapM_ outputStrLn err1
          loop o l m
        (_, Right (err2, _)) -> do
          mapM_ outputStrLn err2
          loop o l m

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
parseAndType us p tc s = case parse (liftM2 const (spaces >> p) eof) "" s of
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
