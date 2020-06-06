module Rcl.ToOcl (ocl, aggName, tr, enc) where

import Data.Char (isAlphaNum, isAscii, ord, toLower, toUpper)
import Data.Either (rights)
import Data.Map (differenceWith, toList)
import qualified Data.Set as Set
import Numeric (showHex)

import Rcl.Ast
import Rcl.Reduce (runReduce)
import Rcl.Type (isElem, mBaseType, wellTyped)

import Text.PrettyPrint

toUse :: UserTypes -> [String]
toUse us = let
  l = toList $ differenceWith (\ s1 s2 ->
    let s = Set.difference s1 s2 in
    if Set.null s then Nothing else Just s) us builtinTypes in
  concatMap toSetClass (Set.unions $ map (toSubs . snd) l)
  ++ concatMap toClass l ++ ["class RBAC < Builtin", "operations"]
  ++ concatMap toOp l ++ [end, "constraints", "context RBAC"]

toSubs :: Set.Set SetType -> Set.Set SetType
toSubs = Set.unions . map toSubsAux . Set.toList

toSubsAux :: SetType -> Set.Set SetType
toSubsAux t = case t of
  ElemTy _ -> Set.empty
  SetOf s -> Set.insert t $ toSubsAux s

-- | translate user _c_onflict set names
tr :: SetType -> String -> String
tr t s = let b = baseType t in if (b, s) `elem` primTypes && t == toSet b
  then s else 'c' : stSet t ++ '_' : enc s

-- | code out non-valid characters
enc :: String -> String
enc = concatMap $ \ c -> case c of
  '_' -> "__"
  _ | isAscii c && isAlphaNum c -> [c]
    | otherwise -> '_' : map toUpper (showHex (ord c) "_")

toClass :: (String, Set.Set SetType) -> [String]
toClass (s, ts) = map (toClassAux s) $ Set.toList ts

toClassAux :: String -> SetType -> String
toClassAux s t = "class " ++ tr t s ++ " < " ++ stSet t ++ " end"

toOp :: (String, Set.Set SetType) -> [String]
toOp (s, ts) = map (toOpAux s) $ Set.toList ts

toOpAux :: String -> SetType -> String
toOpAux s t = let r = tr t s in
  "  " ++ r ++ "() : " ++ useType t ++ " = " ++ r
  ++ ".allInstances->any(true)" ++ if isElem t then "" else ".c()"

toSetClass :: SetType -> [String]
toSetClass t = case t of
  ElemTy _ -> error "toSetClass"
  SetOf s -> let c = stSet t in
    [ "class " ++ c
    , "operations"
    , "  c() : " ++ useType t ++ " = " ++ roleName s
      ++ if isElem s then "" else "->collectNested(c())->asSet"
    , end
    , "aggregation Ag" ++ c ++ " between"
    , "  " ++ c ++ "[*]"
    , "  " ++ stSet s ++ "[*]"
    , end ]

aggName :: SetType -> String
aggName = ("Ag" ++) . stSet

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
    , uncurry toOcl $ runReduce us s]) [1 :: Int ..]
    (rights $ map (wellTyped us) l)

toOcl :: Stmt -> Vars -> Doc
toOcl = foldl (\ f (i, s) -> cat
   [ hcat [setToOcl s, arr, text "forAll"]
   , parens $ sep [text $ stVar i ++ " |", f]]) . stmtToOcl

arr :: Doc
arr = text "->"

stmtToOcl :: Stmt -> Doc
stmtToOcl = foldStmt FoldStmt
  { foldBool = \ (BoolOp _ s1 s2) o d1 d2 ->
      sep [parenStmt s1 d1, pBoolOp o <+> parenStmt s2 d2]
  , foldCmp = \ (CmpOp _ _ s2) o d1 d2 -> case o of
      Elem -> cat [hcat [d2, arr, text "includes"], parens d1]
      Eq | s2 == EmptySet -> hcat [d1, arr, text "isEmpty"]
      Ne | s2 == EmptySet -> hcat [d1, arr, text "notEmpty"]
      _ -> sep [d1, pCmpOp o <+> d2] }
  termToOcl

parenStmt :: Stmt -> Doc -> Doc
parenStmt s = case s of
  BoolOp Impl _ _ -> parens
  _ -> id

termToOcl :: Term -> Doc
termToOcl t = case t of
  Term b s -> let d = setToOcl s in case b of
    Card -> hcat [d, arr, text "size"]
    TheSet -> d
  EmptySet -> text "Set{}" -- never possible see isEmpty and notEmpty
  Num i -> int i

setToOcl :: Set -> Doc
setToOcl = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 _) o a1 a2 -> let
      p = text $ useBinOp (mBaseType s1) o
      in case o of
      Operations _ -> cat [p, parens $ hcat [a1, text ",", a2]]
      Minus -> parens $ hcat [a1, p, a2]
      _ -> cat [hcat [a1, arr, p], parens a2]
  , foldUn = \ (UnOp _ s) o d -> case o of
      Typed _ ts -> case getUntypedSet s of
        PrimSet t -> case Set.minView ts of
          Just (e, r) | Set.null r -> text $ tr e t ++ "()"
          _ -> error "setToOcl: prim set unknown"
        _ -> d
      _ -> cat [text $ useOp (mBaseType s) o, parens d]
  , foldBraced = \ _ ds -> hcat [text "Set", braces . fcat $ punctuate comma ds]
  , foldPrim = \ s -> text $ case s of
      Var (MkVar i t _) -> t ++ show i
      _ -> error "setToOcl: no prim set" }

pBoolOp :: BoolOp -> Doc
pBoolOp o = text $ case o of
  And -> "and"
  Impl -> "implies"

pCmpOp :: CmpOp -> Doc
pCmpOp o = text $ case o of
  Ne -> "<>"
  _ -> sCmpOp Ascii o

useBinOp :: Set.Set Base -> BinOp -> String
useBinOp t o = case o of
  Union -> "union"
  Inter -> "intersection"
  Minus -> "-"
  Operations b -> let s = "ops" ++ optStar b in case Set.minView t of
    Just (r, _) -> map toLower (show r) ++ s
    _ -> s

-- | USE compliant and disambiguated names
useOp :: Set.Set Base -> UnOp -> String
useOp t o = let u = map (\ c -> if c == '*' then '_' else c) $ stUnOp o
  in case o of
  User _ b -> case Set.minView t of
    Just (S, r) | Set.null r -> "user"
    _ -> "users" ++ optStar b
  Object _ -> "objects"
  Roles _ -> case Set.minView t of
      Just (r, _) -> map toLower (show r) ++ u
      _ -> u
  Permissions _ -> case Set.minView t of
      Just (r, _) -> map toLower (show r) ++ u
      _ -> u
  Iors i b -> map toLower (show i) ++ 's' : optStar b
  _ -> u

optStar :: OptStar -> String
optStar b = case b of
  Star -> "_"
  _ -> ""
