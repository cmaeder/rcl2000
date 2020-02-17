module Rcl.Reduce (reduction, Vars, runReduce) where

import Control.Applicative ((<|>))
import Control.Exception (assert)
import Control.Monad.State (State, modify, runState)
import Rcl.Ast
import Rcl.Print
import Rcl.Type

data FoldSet a = FoldSet
  { foldBin :: Set -> BinOp -> a -> a -> a
  , foldUn :: Set -> UnOp -> a -> a
  , foldPrim :: Set -> a }

mapSet :: FoldSet Set
mapSet = FoldSet
  { foldBin = const BinOp
  , foldUn = const UnOp
  , foldPrim = id }

const2 :: a -> b -> c -> a
const2 = const . const

constSet :: (a -> a -> a) -> a -> FoldSet a
constSet f a = FoldSet
  { foldBin = const2 f
  , foldUn = const2 id
  , foldPrim = const a }

foldSet :: FoldSet a -> Set -> a
foldSet r s = case s of
  BinOp o s1 s2 -> foldBin r s o (foldSet r s1) $ foldSet r s2
  UnOp o p -> foldUn r s o $ foldSet r p
  _ -> foldPrim r s

data FoldStmt a b = FoldStmt
  { foldBool :: Stmt -> BoolOp -> b -> b -> b
  , foldCmp :: Stmt -> CmpOp -> a -> a -> b }

mapStmt :: FoldStmt Set Stmt
mapStmt = FoldStmt
  { foldBool = const BoolOp
  , foldCmp = const CmpOp }

constStmt :: (b -> b -> b) -> (a -> a -> b) -> FoldStmt a b
constStmt f g = FoldStmt
  { foldBool = const2 f
  , foldCmp = const2 g }

foldStmt :: FoldStmt a b -> (Set -> a) -> Stmt -> b
foldStmt r f s = case s of
  BoolOp o s1 s2 -> foldBool r s o (foldStmt r f s1) $ foldStmt r f s2
  CmpOp o s1 s2 -> foldCmp r s o (f s1) $ f s2

replaceAO :: Stmt -> Stmt
replaceAO = foldStmt mapStmt replAO

replAO :: Set -> Set
replAO = foldSet mapSet
  { foldUn = \ _ o p -> case o of
      AO -> BinOp Minus p (UnOp OE p)
      _ -> UnOp o p }

findSimpleOE :: Stmt -> Maybe Set
findSimpleOE = foldStmt (constStmt (<|>) (<|>)) findOE

findOE :: Set -> Maybe Set
findOE = foldSet (constSet (<|>) Nothing)
  { foldUn = \ s _ r -> r <|> case s of
      UnOp OE p -> Just p -- we omit the outer OE
      _ -> Nothing }

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
    reduce (i + 1) $ replaceOE r (Var i $ typeOfSet r) s

runReduce :: Stmt -> (Stmt, Vars)
runReduce s = runState (reduce 1 $ replaceAO s) []

construct :: Stmt -> Vars -> Stmt
construct = foldl (\ r (i, t) -> replaceVar i t r)

replaceVar :: Int -> Set -> Stmt -> Stmt
replaceVar i = foldStmt mapStmt . replVar i

replVar :: Int -> Set -> Set -> Set
replVar i r = foldSet mapSet
  { foldPrim = \ s -> case s of
    Var j t | i == j -> assert (typeOfSet r == t) $ UnOp OE r
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
