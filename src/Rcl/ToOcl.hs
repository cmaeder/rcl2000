module Rcl.ToOcl (ocl, aggName, tr, enc) where

import Data.Char (isAlphaNum, isAscii, isAsciiUpper, isDigit, ord, toLower,
                  toUpper)
import Data.Either (rights)
import Data.Map (filterWithKey, findWithDefault, toList)
import qualified Data.Set as Set
import Numeric (showHex)

import Rcl.Ast
import Rcl.Reduce (runReduce)
import Rcl.Type (isElem, mBaseType, wellTyped)

import Text.PrettyPrint

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
    (rights $ map (wellTyped us) l)

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
      _ -> let (a1, a2) = singleTerm s1 s2 d1 d2 in
        sep [a1, pCmpOp o <+> a2] }
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

singleTerm :: Term -> Term -> Doc -> Doc -> (Doc, Doc)
singleTerm t1 t2 d1 d2 = case (t1, t2) of
  (Term TheSet s1, Term TheSet s2) -> let (b1, b2) = cast s1 s2 in
    (singleton b1 d1, singleton b2 d2)
  _ -> (d1, d2)

setToOcl :: UserTypes -> Set -> Doc
setToOcl = setToOclAux

setToOclAux :: UserTypes -> Set -> Doc
setToOclAux us = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 s2) o d1 d2 -> let
      p = text $ useBinOp (mBaseType s1) o
      (b1, b2) = cast s1 s2
      c1 = elemType s1
      c2 = elemType s2
      fs b c = singleton $ case o of
        Operations _ -> c
        _ -> b || c
      a1 = fs b1 c1 d1
      a2 = fs b2 c2 d2
      in case o of
      Operations _ -> cat [p, parens $ hcat [a1, text ",", a2]]
      Minus -> parens $ hcat [a1, p, a2]
      _ -> cat [hcat [a1, arr, p], parens a2]
  , foldUn = \ (UnOp _ s) o d -> case o of
      Typed _ _ -> d
      _ -> let p = useOp (mBaseType s) o in
        cat [text p, parens $ if p == "user" then d else singleSet s d]
  , foldBraced = \ _ ds -> hcat [text "Set", braces . fcat $ punctuate comma ds]
  , foldPrim = \ s -> text $ case s of
      PrimSet t -> let ts = findWithDefault Set.empty t us in
        case Set.minView ts of
          Just (e, r) -> tr (if Set.null r then Nothing else Just e) t ++ "()"
          _ -> error "setToOcl: prim set unknown"
      Var (MkVar i t _) -> t ++ show i
      _ -> error "setToOcl: no prim set" }

cast :: Set -> Set -> (Bool, Bool)
cast s1 s2 =
  let ts1 = getType s1
      ts2 = getType s2
      f ts = any ((`Set.member` ts) . SetOf) . Set.toList
      b1 = f ts2 ts1
      b2 = f ts1 ts2
  in (b1, b2)

singleSet :: Set -> Doc -> Doc
singleSet = singleton . elemType

elemType :: Set -> Bool
elemType s = case Set.toList $ getType s of
  m : _ -> isElem m
  _ -> False

singleton :: Bool -> Doc -> Doc
singleton b d = if b then hcat [text "Set", braces d] else d

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
