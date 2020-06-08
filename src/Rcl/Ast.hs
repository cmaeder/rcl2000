module Rcl.Ast where

import Data.Char (isLetter, toLower)
import qualified Data.Map as Map (Map, empty, insert)
import qualified Data.Set as Set (Set, empty, map, singleton, toList)

data Stmt = CmpOp CmpOp Term Term -- named expression by Ahn
  | BoolOp BoolOp Stmt Stmt deriving (Eq, Show)

data FoldStmt a b = FoldStmt
  { foldBool :: Stmt -> BoolOp -> b -> b -> b
  , foldCmp :: Stmt -> CmpOp -> a -> a -> b }

data CmpOp = Elem | Eq | Le | Lt | Ge | Gt | Ne deriving (Eq, Show)
data BoolOp = And | Impl deriving (Eq, Show)

data Term = Term OptCard Set | EmptySet | Num Int deriving (Eq, Show)
-- named token by Ahn without size, the cardinality of sets is optional

data OptCard = Card | TheSet deriving (Eq, Show)

data Set = PrimSet String | UnOp UnOp Set | Braced [Set]
  | BinOp BinOp Set Set | Var Var deriving (Eq, Show) -- named term by Ahn

data FoldSet a = FoldSet
  { foldBin :: Set -> BinOp -> a -> a -> a
  , foldUn :: Set -> UnOp -> a -> a
  , foldBraced :: Set -> [a] -> a
  , foldPrim :: Set -> a }

data Var = MkVar Int String (Set.Set SetType) deriving (Eq, Show)

data BinOp = Union | Inter | Operations OptStar | Minus deriving (Eq, Show)
-- operations is special binary and Minus is used for reduction of AO

data OptStar = Star | TheOp deriving (Eq, Show)

data UnOp = Typed Annotation (Set.Set SetType)
  | AO | OE | User OptS OptStar | Roles OptStar | Sessions
  | Permissions OptStar | Object OptS | Iors Ior OptStar deriving (Eq, Show)
-- AO: all other, OE: one element, optional s and/or * suffix

data Annotation = Explicit | Derived deriving Show
instance Eq Annotation where
  _ == _ = True

data OptS = Plural | Singular deriving Show
instance Eq OptS where
  _ == _ = True

data Ior = Jun | Sen deriving (Eq, Show)

data Base = U | R | OP | OBJ | P | S deriving (Eq, Ord, Show)
data SetType = ElemTy Base | SetOf SetType deriving (Eq, Ord, Show)
data Format = Ascii | Uni | LaTeX deriving (Eq, Show)
type UserTypes = Map.Map String (Set.Set SetType)
type Vars = [(Var, Set)]

primTypes :: [(Base, String)]
primTypes = let bs = [U, R, OP, OBJ, P, S] in zip bs $ map show bs

builtinTypes :: UserTypes
builtinTypes = foldr
  (\ (b, s) -> Map.insert s . Set.singleton $ toSet b) Map.empty primTypes

toSet :: Base -> SetType
toSet = SetOf . ElemTy

forms :: [Format]
forms = [Ascii, Uni, LaTeX]

foldStmt :: FoldStmt a b -> (Term -> a) -> Stmt -> b
foldStmt r f s = case s of
  BoolOp o s1 s2 -> foldBool r s o (foldStmt r f s1) $ foldStmt r f s2
  CmpOp o s1 s2 -> foldCmp r s o (f s1) $ f s2

foldSet :: FoldSet a -> Set -> a
foldSet r s = case s of
  BinOp o s1 s2 -> foldBin r s o (foldSet r s1) $ foldSet r s2
  UnOp o p -> foldUn r s o $ foldSet r p
  Braced ss -> foldBraced r s $ map (foldSet r) ss
  _ -> foldPrim r s

untyped :: Set -> Set
untyped s = case s of
  UnOp (Typed _ _) e -> e
  _ -> s

typeOf :: Set -> Set.Set SetType
typeOf s = case s of
  UnOp (Typed _ ts) _ -> ts
  Var (MkVar _ _ ts) -> ts
  _ -> Set.empty

foldSetType :: (a -> a) -> (Base -> a) -> SetType -> a
foldSetType f g s = case s of
  ElemTy b -> g b
  SetOf t -> f $ foldSetType f g t

baseType :: SetType -> Base
baseType = foldSetType id id

mBaseType :: Set -> Set.Set Base
mBaseType = Set.map baseType . typeOf

stVar :: Var -> String
stVar (MkVar i t _) = t ++ show i

sEmpty :: Format -> String
sEmpty m = case m of
  LaTeX -> "\\emptyset"
  Uni -> "\x2205"
  Ascii -> "{}"

sUnion :: Format -> String
sUnion m = case m of
  LaTeX -> "\\cup"
  Uni -> "\x222A"
  Ascii -> "+"

sInter :: Format -> String
sInter m = case m of
  LaTeX -> "\\cap"
  Uni -> "\x2229"
  Ascii -> "&"

stUnOp :: Show a => a -> String
stUnOp o = let
  s = show o -- rely on Show instance
  ws = words s
  l = length s
  v = map toLower s
  w = takeWhile isLetter v
  add e c = if e `elem` ws then (++ [c]) else id
  x = if w == "iors" then (if "Jun" `elem` ws then "jun" else "sen") ++ w
    else add "Plural" 's' w
  in if l == 2 then s else -- not AO or OE
  add "Star" '*' x

lUnOp :: Show a => a -> String
lUnOp o = case stUnOp o of
  s@(_ : _) | last s == '*' -> init s ++ "^{*}"
  s -> s

sCmpOp :: Format -> CmpOp -> String
sCmpOp m o = case m of
  Ascii -> case o of
    Elem -> "in"
    Eq -> "="
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"
    Ne -> "/="
  Uni -> case o of
    Elem -> "\x2208"
    Le -> "\x2264"
    Ge -> "\x2265"
    Ne -> "\x2260"
    _ -> sCmpOp Ascii o
  LaTeX -> case o of
    Elem -> "\\in"
    Le -> "\\leq"
    Ge -> "\\geq"
    Ne -> "\\neq"
    _ -> sCmpOp Ascii o

sAnd :: Format -> String
sAnd m = case m of
  LaTeX -> "\\land"
  Uni -> "\x2227"
  Ascii -> "/\\"

sImpl :: Format -> String
sImpl m = case m of
  LaTeX -> "\\Rightarrow"
  Uni -> "\x21D2"
  Ascii -> "=>"

sAll :: Format -> String
sAll m = case m of
  LaTeX -> "\\forall{}"
  Uni -> "\x2200"
  Ascii -> "forAll "

sIn :: Format -> String
sIn m = case m of
  LaTeX -> "\\in{}"
  Uni -> "\x220A"
  Ascii -> ":"

sDot :: Format -> String
sDot m = case m of
  LaTeX -> "\\cdot{}"
  Uni -> "\x2219"
  Ascii -> "."

-- | list of unicode characters legal in statement
keySigns :: String
keySigns = concatMap (\ f -> f Uni) [sAnd, sImpl, sUnion, sInter, sEmpty]
  ++ concatMap (sCmpOp Uni) [Elem, Le, Ge, Ne]

-- | OCL compliant class name
stSet :: SetType -> String
stSet = foldSetType (++ "s") show

ppType :: Set.Set SetType -> String
ppType s = case Set.toList s of
  [] -> "Unknown"
  [t] -> stSet t
  ts -> unwords $ map stSet ts
