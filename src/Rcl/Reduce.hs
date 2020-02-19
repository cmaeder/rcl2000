module Rcl.Reduce (reduction, Vars, runReduce) where

import Control.Applicative ((<|>))
import Control.Exception (assert)
import Control.Monad.State (State, modify, runState)
import Rcl.Ast
import Rcl.Fold
import Rcl.Print (ppStmt, ppSet)
import Rcl.Type (typeOfSet, elemType)

replaceAO :: Stmt -> Stmt
replaceAO = foldStmt mapStmt replAO

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
  , foldCmp = const2 (<|>) } findOE

findOE :: Set -> Maybe Set
findOE = foldSet FoldSet
  { foldBin = const2 (<|>)
  , foldUn = \ s _ r -> r <|> case s of
      UnOp OE p -> Just p -- we omit the outer OE
      _ -> Nothing
  , foldPrim = const Nothing }

replaceOE :: Set -> Set -> Stmt -> Stmt
replaceOE e = foldStmt mapStmt . replOE e

replOE :: Set -> Set -> Set -> Set
replOE e r = foldSet mapSet
  { foldUn = \ _ o p -> case o of
      OE | p == e -> r
      _ -> UnOp o p }

type Vars = [(Int, Set)]

reduce :: Int -> Stmt -> State Vars Stmt
reduce i s = case findSimpleOE s of
  Nothing -> pure s
  Just r -> do
    modify ((i, r) :)
    reduce (i + 1) $ replaceOE r (Var i . elemType $ typeOfSet r) s

runReduce :: Stmt -> (Stmt, Vars)
runReduce s = runState (reduce 1 $ replaceAO s) []

construct :: Stmt -> Vars -> Stmt
construct = foldl (\ r (i, t) -> replaceVar i t r)

replaceVar :: Int -> Set -> Stmt -> Stmt
replaceVar i = foldStmt mapStmt . replVar i

replVar :: Int -> Set -> Set -> Set
replVar i r = foldSet mapSet
  { foldPrim = \ s -> case s of
    Var j t | i == j -> assert (elemType (typeOfSet r) == t) $ UnOp OE r
    _ -> s }

replaceMinus :: Stmt -> Stmt
replaceMinus = foldStmt mapStmt replMinus

replMinus :: Set -> Set
replMinus = foldSet mapSet
  { foldBin = \ _ o s1 s2 -> case o of
      Minus -> assert (s2 == UnOp OE s1) $ UnOp AO s1
      _ -> BinOp o s1 s2 }

reduceAndReconstruct :: Stmt -> String
reduceAndReconstruct s = let
  (r, vs) = runReduce s
  n = replaceMinus (construct r vs)
  in assert (n == s)
  $ concatMap (\ (i, e) -> 'v' : show i ++ ":" ++ ppSet e ++ ";")
    (reverse vs) ++ ppStmt r

reduction :: [Stmt] -> String
reduction = unlines . map reduceAndReconstruct
