module Rcl.Reduce where

import Control.Applicative
import Control.Monad.State
import Rcl.Ast
import Rcl.Print
import Rcl.Type

data FoldSet a = FoldSet
  { foldBin :: Set -> BinOp -> a -> a -> a
  , foldUn :: Set -> UnOp -> a -> a }

mapSet :: FoldSet Set
mapSet = FoldSet
  { foldBin = const BinOp
  , foldUn = const UnOp }

const2 :: a -> b -> c -> a
const2 = const . const

constSet :: (a -> a -> a) -> FoldSet a
constSet f = FoldSet
  { foldBin = const2 f
  , foldUn = const2 id }

foldSet :: FoldSet a -> (Set -> a) -> Set -> a
foldSet r f s = case s of
  BinOp o s1 s2 -> foldBin r s o (foldSet r f s1) $ foldSet r f s2
  UnOp o p -> foldUn r s o $ foldSet r f p
  _ -> f s

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
      _ -> UnOp o p } id

findSimpleOE :: Stmt -> Maybe Set
findSimpleOE = foldStmt (constStmt (<|>) (<|>)) findOE

findOE :: Set -> Maybe Set
findOE = foldSet (constSet (<|>))
  { foldUn = \ s _ r -> r <|> case s of
      UnOp OE p -> Just p -- we omit the outer OE
      _ -> Nothing } $ const Nothing

replaceOE :: Set -> Set -> Stmt -> Stmt
replaceOE e = foldStmt mapStmt . replOE e

replOE :: Set -> Set -> Set -> Set
replOE e r = foldSet mapSet
  { foldUn = \ _ o p -> case o of
      OE | p == e -> r
      _ -> UnOp o p } id

type Vars = [(Int, Set)]

reduce :: Int -> Stmt -> State Vars Stmt
reduce i s = case findSimpleOE s of
  Nothing -> pure s
  Just r -> do
    modify ((i, r) :)
    reduce (i + 1) $ replaceOE r (Var i $ typeOfSet r) s

runReduce :: Stmt -> (Stmt, Vars)
runReduce s = runState (reduce 1 $ replaceAO s) []

printReduce :: Stmt -> String
printReduce s = let (r, vs) = runReduce s in
  concatMap (\ (i, e) -> 'v' : show i ++ ":" ++ ppSet e ++ ";")
    (reverse vs) ++ ppStmt r
