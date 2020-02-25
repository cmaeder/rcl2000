module Rcl.Reduce (reduction, Vars, runReduce) where

import Control.Applicative ((<|>))
import Control.Exception (assert)
import Control.Monad.State (State, modify, runState)
import Data.Char (toLower)
import Rcl.Ast
import Rcl.Fold
import Rcl.Print (ppStmt, ppSet)
import Rcl.Type (typeOfSet, elemType)

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
  , foldPrim = const Nothing }

replaceOE :: Set -> Set -> Stmt -> Stmt
replaceOE e = foldStmt mapStmt . mapTerm . replOE e

replOE :: Set -> Set -> Set -> Set
replOE e r = foldSet mapSet
  { foldUn = \ _ o p -> case o of
      OE | p == e -> r
      _ -> UnOp o p }

type Vars = [(Var, Set)]

reduce :: UserTypes -> Int -> Stmt -> State Vars Stmt
reduce us i s = case findSimpleOE s of
  Nothing -> pure s
  Just r -> do
    let mt = typeOfSet us r >>= elemType
        p = case mt of
          Just (ElemTy e) -> show e
          _ -> ppSet r
        v = MkVar i (take 2 $ map toLower p) mt
    modify ((v, r) :)
    reduce us (i + 1) $ replaceOE r (Var v) s

runReduce :: UserTypes -> Stmt -> (Stmt, Vars)
runReduce us s = runState (reduce us 1 $ replaceAO s) []

construct :: UserTypes -> Stmt -> Vars -> Stmt
construct us = foldl (\ r (i, t) -> replaceVar us i t r)

replaceVar :: UserTypes -> Var -> Set -> Stmt -> Stmt
replaceVar us i = foldStmt mapStmt . mapTerm . replVar us i

replVar :: UserTypes -> Var -> Set -> Set -> Set
replVar us i@(MkVar _ _ t) r =
  assert ((typeOfSet us r >>= elemType) == t) . foldSet mapSet
  { foldPrim = \ s -> case s of
    Var v | i == v -> UnOp OE r
    _ -> s }

replaceMinus :: Stmt -> Stmt
replaceMinus = foldStmt mapStmt $ mapTerm replMinus

replMinus :: Set -> Set
replMinus = foldSet mapSet
  { foldBin = \ _ o s1 s2 -> case o of
      Minus -> assert (s2 == UnOp OE s1) $ UnOp AO s1
      _ -> BinOp o s1 s2 }

reduceAndReconstruct :: UserTypes -> Stmt -> String
reduceAndReconstruct us s = let
  (r, vs) = runReduce us s
  n = replaceMinus (construct us r vs)
  in assert (n == s)
  $ concatMap (\ (i, e) ->
      '\x2200' : stVar i ++ '\x220A' : ppSet e ++ "\x2219")
    (reverse vs) ++ ' ' : ppStmt r

reduction :: UserTypes -> [Stmt] -> String
reduction us = unlines . map (reduceAndReconstruct us)
