module Rcl.Ast where

import Data.Char (isLetter, toLower)
import Data.List (isSuffixOf)
import Data.Map (Map)

data Stmt = CmpOp CmpOp Term Term -- named expression by Ahn
  | BoolOp BoolOp Stmt Stmt deriving (Eq, Show)

data FoldStmt a b = FoldStmt
  { foldBool :: Stmt -> BoolOp -> b -> b -> b
  , foldCmp :: Stmt -> CmpOp -> a -> a -> b }

data CmpOp = Elem | Eq | Le | Lt | Ge | Gt | Ne deriving (Eq, Show)
data BoolOp = And | Impl deriving (Eq, Show)

data Term = Term Bool Set | EmptySet | Num Int deriving (Eq, Show)
-- named token by Ahn without size, Bool for cardinality

data Set = PrimSet { stPrim :: String } | UnOp UnOp Set
  | BinOp BinOp Set Set | Var Var deriving (Eq, Show) -- named term by Ahn

data FoldSet a = FoldSet
  { foldBin :: Set -> BinOp -> a -> a -> a
  , foldUn :: Set -> UnOp -> a -> a
  , foldPrim :: Set -> a }

data Var = MkVar Int String (Maybe SetType) deriving (Eq, Show)

data BinOp = Union | Inter | Ops | Minus deriving (Eq, Show)
-- operations is special binary and Minus is used for reduction of AO

data UnOp = AO | OE | User Bool | Roles Bool | Sessions
  | Permissions Bool | Objects deriving (Eq, Show)
-- AO: all other, OE: one element, object ~> objects, Bool for * suffix

data Base = U | R | OP | OBJ | P | S deriving (Eq, Ord, Show)
data SetType = ElemTy Base | SetOf SetType deriving (Eq, Show)
data Type = SetTy SetType | NatTy | EmptySetTy deriving (Eq, Show)
data Format = Ascii | Uni | LaTeX deriving (Eq, Show)
type UserTypes = Map String SetType
type Vars = [(Var, Set)]

primTypes :: [Base]
primTypes = [U, R, OP, OBJ, P, S]

forms :: [Format]
forms = [Ascii, Uni, LaTeX]

mapStmt :: FoldStmt Term Stmt
mapStmt = FoldStmt
  { foldBool = const BoolOp
  , foldCmp = const CmpOp }

foldStmt :: FoldStmt a b -> (Term -> a) -> Stmt -> b
foldStmt r f s = case s of
  BoolOp o s1 s2 -> foldBool r s o (foldStmt r f s1) $ foldStmt r f s2
  CmpOp o s1 s2 -> foldCmp r s o (f s1) $ f s2

mapTerm :: (Set -> Set) -> Term -> Term
mapTerm f t = case t of
  Term b s -> Term b $ f s
  _ -> t

mapSet :: FoldSet Set
mapSet = FoldSet
  { foldBin = const BinOp
  , foldUn = const UnOp
  , foldPrim = id }

foldSet :: FoldSet a -> Set -> a
foldSet r s = case s of
  BinOp o s1 s2 -> foldBin r s o (foldSet r s1) $ foldSet r s2
  UnOp o p -> foldUn r s o $ foldSet r p
  _ -> foldPrim r s

foldSetType :: (a -> a) -> (Base -> a) -> SetType -> a
foldSetType f g s = case s of
  ElemTy b -> g b
  SetOf t -> f $ foldSetType f g t

baseType :: SetType -> Base
baseType = foldSetType id id

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

stOps :: String
stOps = "operations"

unOps :: [UnOp]
unOps = [AO, OE, User True, User False, Roles True, Roles False, Sessions
  , Permissions True, Permissions False, Objects]

stUnOp :: UnOp -> String
stUnOp o = let
  s = show o -- rely on Show instance
  l = length s
  v = map toLower s
  w = takeWhile isLetter v
  in if l == 2 then s else -- not AO or OE
  if " True" `isSuffixOf` s then w ++ "*" else w

lUnOp :: UnOp -> String
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
stSet = foldSetType ("SetOf" ++) show

-- | USE compliant and disambiguated names
useOp :: Maybe Base -> UnOp -> String
useOp t o = let u = map (\ c -> if c == '*' then '_' else c) $ stUnOp o
  in case o of
  User b -> if t == Just S then u else if b then "users_" else "users"
  Roles _ -> case t of
      Just r -> map toLower (show r) ++ u
      _ -> u
  _ -> u
