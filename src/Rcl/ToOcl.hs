module Rcl.ToOcl (ocl, aggName, tr, enc) where

import Data.Char (isAlphaNum, isAscii, isAsciiUpper, isDigit, ord, toLower,
                  toUpper)
import Data.Map (filterWithKey, findWithDefault, toList)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Numeric (showHex)

import Rcl.Ast
import Rcl.Reduce (runReduce)
import Rcl.Type (isElem, mBaseType, typeOfSet, wellTyped)

import Text.PrettyPrint (Doc, braces, cat, hcat, int, parens, render, sep, text,
                         (<+>))

toUse :: UserTypes -> [String]
toUse us = let
  l = toList $ filterWithKey
    (\ k _ -> k `notElem` map show primTypes) us in
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
tr :: Maybe SetType -> String -> String
tr mt s = case mt of
  Nothing -> if s `elem` map show primTypes
    || all (\ c -> isDigit c || isAsciiUpper c) s
    && s `notElem` words "RRAC RH UA PA HR SU"
    then s else "c_" ++ enc s
  Just t -> 'c' : stSet t ++ '_' : enc s

-- | code out non-valid characters
enc :: String -> String
enc = concatMap $ \ c -> case c of
  '_' -> "__"
  _ | isAscii c && isAlphaNum c -> [c]
    | otherwise -> '_' : map toUpper (showHex (ord c) "_")

toClass :: (String, Set.Set SetType) -> [String]
toClass (s, ts) = case Set.minView ts of
  Just (t, r) | Set.null r -> [toClassAux True s t]
  _ -> map (toClassAux False s) $ Set.toList ts

toClassAux :: Bool -> String -> SetType -> String
toClassAux b s t = "class " ++ tr (if b then Nothing else Just t) s
  ++ " < " ++ stSet t ++ " end"

toOp :: (String, Set.Set SetType) -> [String]
toOp (s, ts) = case Set.minView ts of
  Just (t, r) | Set.null r -> [toOpAux True s t]
  _ -> map (toOpAux False s) $ Set.toList ts

toOpAux :: Bool -> String -> SetType -> String
toOpAux b s t = let r = tr (if b then Nothing else Just t) s in
  "  " ++ r ++ "() : " ++ useType t ++ " = " ++ r
  ++ ".allInstances->any(true).c()"

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
  Term b s -> let d = setToOcl us s in case b of
    Card -> hcat [d, arr, text "size"]
    TheSet -> d
  EmptySet -> text "Set{}" -- never possible see isEmpty and notEmpty
  Num i -> int i

singleTerm :: UserTypes -> Term -> Doc -> Doc
singleTerm us t = case t of
  Term TheSet s -> singleSet us s
  _ -> id

setToOcl :: UserTypes -> Set -> Doc
setToOcl = setToOclAux

setToOclAux :: UserTypes -> Set -> Doc
setToOclAux us = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> let
      p = text $ useBinOp (mBaseType us s1) o
      a1 = singleSet us s1 d1
      a2 = singleSet us s2 d2
      in case o of
      Operations _ -> cat [p, parens $ hcat [a1, text ",", a2]]
      Minus -> parens $ hcat [a1, p, a2]
      _ -> cat [hcat [a1, arr, p], parens a2]
  , foldUn = \ (UnOp _ s) o d -> let p = useOp (mBaseType us s) o in
        cat [text p, parens $ if p == "user" then d else singleSet us s d]
  , foldPrim = \ s -> text $ case s of
      PrimSet t -> let ts = findWithDefault Set.empty t us in
        case Set.minView ts of
          Just (e, r) -> tr (if Set.null r then Nothing else Just e) t ++ "()"
          _ -> error "setToOcl: prim set unknown"
      Var (MkVar i t _) -> t ++ show i
      _ -> error "setToOcl: no prim set" }

singleSet :: UserTypes -> Set -> Doc -> Doc
singleSet us = singleSetType . typeOfSet us

singleSetType :: Set.Set SetType -> Doc -> Doc
singleSetType t d = case Set.minView t of
  Just (m, s) | Set.null s && isElem m -> hcat [text "Set", braces d]
  _ -> d

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
