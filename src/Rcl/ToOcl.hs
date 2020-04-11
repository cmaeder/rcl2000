module Rcl.ToOcl (ocl, aggName, tr, enc) where

import Data.Char (ord, toLower, toUpper,
  isDigit, isAsciiUpper, isAscii, isAlphaNum)
import Data.List (nub)
import Data.Map (toList)
import Data.Maybe (isNothing)
import Numeric (showHex)

import Rcl.Ast
import Rcl.Reduce (runReduce)
import Rcl.Type (wellTyped, typeOfSet, isElem)
import Text.PrettyPrint (Doc, render, text, (<+>), hcat, cat, sep,
  parens, braces, int)

toUse :: UserTypes -> [String]
toUse us = let l = toList us in
  concatMap toSetClass (nub $ concatMap (toSubs . snd) l)
  ++ map toClass l ++ ["class RBAC < Builtin", "operations"]
  ++ map toOp l ++ [end, "constraints", "context RBAC"]

toSubs :: SetType -> [SetType]
toSubs t = case t of
  ElemTy _ -> []
  SetOf s -> t : toSubs s

-- | translate user _c_onflict set names
tr :: String -> String
tr s = if s `elem` map show primTypes
  || all (\ c -> isDigit c || isAsciiUpper c) s
  && s `notElem` words "RRAC RH UA PA HR SU"
  then s else "c_" ++ enc s

-- | code out non-valid characters
enc :: String -> String
enc = concatMap $ \ c -> case c of
  '_' -> "__"
  _ | isAscii c && isAlphaNum c -> [c]
    | otherwise -> '_' : map toUpper (showHex (ord c) "_")

toClass :: (String, SetType) -> String
toClass (s, t) = "class " ++ tr s ++ " < " ++ stSet t ++ " end"

toOp :: (String, SetType) -> String
toOp (s, t) = "  " ++ tr s ++ "() : " ++ useType t ++ " = "
  ++ tr s ++ ".allInstances->any(true).c()"

toSetClass :: SetType -> [String]
toSetClass t = case t of
  ElemTy _ -> error "toSetClass"
  SetOf s -> let c = stSet t in
    [ "class " ++ c
    , "operations"
    , "  c() : " ++ useType t ++ " = " ++ roleName s
      ++ if isElem s then "" else "->collectNested(c())->asSet"
    , end
    , "aggregation A" ++ c ++ " between"
    , "  " ++ c ++ "[*]"
    , "  " ++ stSet s ++ "[*]"
    , end ]

aggName :: SetType -> String
aggName = ('A' :) . stSet

roleName :: SetType -> String
roleName t = case stSet t of
  c : r -> toLower c : r
  "" -> error "roleName"

useType :: SetType -> String
useType = foldSetType (("Set(" ++) . (++ ")")) show

end :: String
end = "end"

ocl :: UserTypes -> [Stmt] -> String
ocl us l = unlines $ toUse us ++ zipWith (\ n s -> render $ hcat
    [ text $ "inv i" ++ show n ++ ": "
    , uncurry (toOcl us) $ runReduce us s]) [1 :: Int ..]
    (filter (isNothing . wellTyped us) l)

toOcl :: UserTypes -> Stmt -> Vars -> Doc
toOcl us = foldl (\ f (i, s) -> cat
   [ hcat [setToOcl us s, arr, text "forAll"]
   , parens $ sep [text $ stVar i ++ " |", f]]) . stmtToOcl us

arr :: Doc
arr = text "->"

stmtToOcl :: UserTypes -> Stmt -> Doc
stmtToOcl us = foldStmt FoldStmt
  { foldBool = \ (BoolOp _ s1 s2) o d1 d2 ->
      sep [parenStmt s1 d1, pBoolOp o <+> parenStmt s2 d2]
  , foldCmp = \ (CmpOp _ s1 s2) o d1 d2 -> case o of
      Elem -> cat [hcat [d2, arr, text "includes"], parens d1]
      Eq | s2 == EmptySet -> hcat [d1, arr, text "isEmpty"]
      Ne | s2 == EmptySet -> hcat [d1, arr, text "notEmpty"]
      _ -> sep [singleTerm us s1 d1, pCmpOp o <+> singleTerm us s2 d2] }
  $ termToOcl us

parenStmt :: Stmt -> Doc -> Doc
parenStmt s = case s of
  BoolOp Impl _ _ -> parens
  _ -> id

termToOcl :: UserTypes -> Term -> Doc
termToOcl us t = case t of
  Term b s -> let d = setToOcl us s in
    if b then hcat [d, arr, text "size"] else d
  EmptySet -> text "Set{}" -- never possible see isEmpty and notEmpty
  Num i -> int i

singleTerm :: UserTypes -> Term -> Doc -> Doc
singleTerm us t = case t of
  Term False s -> singleSet us s
  _ -> id

setToOcl :: UserTypes -> Set -> Doc
setToOcl us = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> let
      p = pBinOp o
      a1 = singleSet us s1 d1
      a2 = singleSet us s2 d2
      in case o of
      Operations _ -> cat [p, parens $ hcat [a1, text ",", a2]]
      Minus -> cat [hcat [d1, arr, p], parens d2]
      _ -> cat [hcat [a1, arr, p], parens a2]
  , foldUn = \ (UnOp _ s) o d ->
        cat [pUnOp (typeOfSet us s) o, parens $ singleSet us s d]
  , foldPrim = \ s -> text $ case s of
      PrimSet t -> tr t ++ "()"
      Var (MkVar i t _) -> t ++ show i
      _ -> error "setToOcl: no prim set" }

singleSet :: UserTypes -> Set -> Doc -> Doc
singleSet us = maybe id singleSetType . typeOfSet us

singleSetType :: SetType -> Doc -> Doc
singleSetType t d =
  if isElem t then hcat [text "Set", braces d] else d

pBoolOp :: BoolOp -> Doc
pBoolOp o = text $ case o of
  And -> "and"
  Impl -> "implies"

pCmpOp :: CmpOp -> Doc
pCmpOp o = text $ case o of
  Ne -> "<>"
  _ -> sCmpOp Ascii o

pBinOp :: BinOp -> Doc
pBinOp o = text $ case o of
  Union -> "union"
  Inter -> "intersection"
  Minus -> "excluding"
  Operations b -> "ops" ++ if b then "_" else ""

pUnOp :: Maybe SetType -> UnOp -> Doc
pUnOp t = text . useOp (fmap (foldSetType id id) t)

-- | USE compliant and disambiguated names
useOp :: Maybe Base -> UnOp -> String
useOp t o = let u = map (\ c -> if c == '*' then '_' else c) $ stUnOp o
  in case o of
  User b -> case t of
    Just S -> u
    _ -> "users" ++ if b then "_" else ""
  Roles _ -> case t of
      Just r -> map toLower (show r) ++ u
      _ -> u
  _ -> u
