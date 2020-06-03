module Rcl.Reduce (reduction, runReduce) where

import Control.Applicative ((<|>))
import Control.Monad.State (State, modify, runState)
import Data.Char (isLetter, toLower)
import qualified Data.Set as Set (toList)

import Rcl.Ast
import Rcl.Print (ppSet, ppStmt, prStmt)
import Rcl.Type (elemType, wellTyped)

mapStmt :: FoldStmt Term Stmt
mapStmt = FoldStmt
  { foldBool = const BoolOp
  , foldCmp = const CmpOp }

mapTerm :: (Set -> Set) -> Term -> Term
mapTerm f t = case t of
  Term b s -> Term b $ f s
  _ -> t

mapSet :: FoldSet Set
mapSet = FoldSet
  { foldBin = const BinOp
  , foldUn = const UnOp
  , foldBraced = const Braced
  , foldPrim = id }

replaceAO :: Stmt -> Stmt
replaceAO = foldStmt mapStmt $ mapTerm replAO

replAO :: Set -> Set
replAO = foldSet mapSet
  { foldUn = \ _ o p -> case o of
      AO -> BinOp Minus p (UnOp OE p)
      _ -> UnOp o p }

const2 :: a -> b -> c -> a
const2 = const . const

findSimpleOE :: Stmt -> Maybe Set
findSimpleOE = foldStmt FoldStmt
  { foldBool = const2 (<|>)
  , foldCmp = const2 (<|>) } findTermOE

findTermOE :: Term -> Maybe Set
findTermOE t = case t of
  Term _ s -> findOE s
  _ -> Nothing

findOE :: Set -> Maybe Set
findOE = foldSet FoldSet
  { foldBin = const2 (<|>)
  , foldUn = \ s _ r -> r <|> case s of
      UnOp OE p -> Just p -- we omit the outer OE
      _ -> Nothing
  , foldBraced = const $ foldl1 (<|>)
  , foldPrim = const Nothing }

replaceOE :: Set -> Set -> Stmt -> Stmt
replaceOE e = foldStmt mapStmt . mapTerm . replOE e

replOE :: Set -> Set -> Set -> Set
replOE e r = foldSet mapSet
  { foldUn = \ _ o p -> case o of
      OE | p == e -> r
      _ -> UnOp o p }

reduce :: UserTypes -> Int -> Stmt -> State Vars Stmt
reduce us i s = case findSimpleOE s of
  Nothing -> pure s
  Just r -> do
    let ts = elemType $ getType r
        p = case Set.toList ts of
          ElemTy e : _ -> show e
          _ -> ppSet $ getUntypedSet r
        v = MkVar i (take 2 . map toLower $ filter isLetter p) ts
    modify ((v, r) :)
    reduce us (i + 1) $ replaceOE r (Var v) s

runReduce :: UserTypes -> Stmt -> (Stmt, Vars)
runReduce us s = runState (reduce us 1 $ replaceAO s) []

construct :: Stmt -> Vars -> Stmt
construct = foldl (flip replaceVar)

checkVar :: (Var, Set) -> String
checkVar (i@(MkVar _ _ t), r) = let s = elemType $ getType r in
  if s == t then ""
  else "type missmatch in variable " ++ stVar i ++ ':' : ppType t
    ++ " versus " ++ ppSet r ++ ':' : ppType s

replaceVar :: (Var, Set) -> Stmt -> Stmt
replaceVar = foldStmt mapStmt . mapTerm . replVar

replVar :: (Var, Set) -> Set -> Set
replVar (i, r) = foldSet mapSet
  { foldPrim = \ s -> case s of
    Var v | i == v -> UnOp OE r
    _ -> s }

replaceMinus :: Stmt -> Stmt
replaceMinus = foldStmt mapStmt $ mapTerm replMinus

replMinus :: Set -> Set
replMinus = foldSet mapSet
  { foldBin = \ _ o s1 s2 -> case o of
      Minus | s2 == UnOp OE s1 -> UnOp AO s1
      _ -> BinOp o s1 s2 }

reduceAndReconstruct :: UserTypes -> Stmt -> [String]
reduceAndReconstruct us so = case wellTyped us so of
  Right s -> let
    p@(r, vs) = runReduce us s
    t = prStmt p
    errs = filter (not . null) $ map checkVar vs
    n = replaceMinus (construct r vs)
    in if null errs then if n == s then [t] else ["given: " ++ ppStmt s
    , "reduced: " ++ t, "reconstructed: " ++ ppStmt n] else errs
  Left e -> [e]

reduction :: UserTypes -> [Stmt] -> String
reduction us = unlines . concatMap (reduceAndReconstruct us)
