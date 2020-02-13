module Rcl.Reduce where

import Control.Applicative
import Control.Monad.State
import Rcl.Ast
import Rcl.Print

replaceAO :: Stmt -> Stmt
replaceAO s = case s of
  CmpOp o s1 s2 -> CmpOp o (replAO s1) $ replAO s2
  BoolOp o s1 s2 -> BoolOp o (replaceAO s1) $ replaceAO s2

replAO :: Set -> Set
replAO s = case s of
  BinOp o s1 s2 -> BinOp o (replAO s1) $ replAO s2
  UnOp o r -> let p = replAO r in
    case o of
      AO -> BinOp Minus p (UnOp OE p)
      _ -> UnOp o p
  _ -> s

findSimpleOE :: Stmt -> Maybe Set
findSimpleOE s = case s of
  CmpOp _ s1 s2 -> findOE s1 <|> findOE s2
  BoolOp _ s1 s2 -> findSimpleOE s1 <|> findSimpleOE s2

findOE :: Set -> Maybe Set
findOE s = case s of
  BinOp _ s1 s2 -> findOE s1 <|> findOE s2
  UnOp o r -> findOE r <|> case o of
    OE -> Just r -- we omit the outer OE
    _ -> Nothing
  _ -> Nothing

replaceOE :: Set -> Set -> Stmt -> Stmt
replaceOE e r s = case s of
  CmpOp o s1 s2 -> CmpOp o (replOE e r s1) $ replOE e r s2
  BoolOp o s1 s2 -> BoolOp o (replaceOE e r s1) $ replaceOE e r s2

replOE :: Set -> Set -> Set -> Set
replOE e r s = case s of
  BinOp o s1 s2 -> BinOp o (replOE e r s1) $ replOE e r s2
  UnOp o t -> let p = replOE e r t in
    case o of
      OE | p == e -> r
      _ -> UnOp o p
  _ -> s

type Vars = [(Int, Set)]

reduce :: Int -> Stmt -> State Vars Stmt
reduce i s = case findSimpleOE s of
  Nothing -> pure s
  Just r -> do
    modify ((i, r) :)
    reduce (i + 1) $ replaceOE r (Var i) s

runReduce :: Stmt -> (Stmt, Vars)
runReduce s = runState (reduce 1 $ replaceAO s) []

printReduce :: Stmt -> String
printReduce s = let (r, vs) = runReduce s in
  concatMap (\ (i, e) -> 'v' : show i ++ ":" ++ ppSet (UnOp OE e) ++ ";")
    (reverse vs) ++ ppStmt r
