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

data UnOp = AO | OE | User | Roles Bool | Sessions
  | Permissions Bool | Objects deriving (Eq, Show)
-- AO: all other, OE: one element, object ~> objects, Bool for * suffix

data Base = U | R | OP | OBJ | P | S deriving (Eq, Ord, Show)

data SetType = ElemTy Base | SetOf SetType deriving (Eq, Show)
data Type = SetTy SetType | NatTy | EmptySetTy deriving (Eq, Show)
type UserTypes = Map String SetType

primTypes :: [Base]
primTypes = [U, R, OP, OBJ, P, S]

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

stVar :: Var -> String
stVar (MkVar i t _) = t ++ show i

chEmpty :: Char
chEmpty = '\x2205'

lEmpty :: String
lEmpty = "\\emptyset"

stEmpty :: String
stEmpty = "{}"

chUnion :: Char
chUnion = '\x222A'

lUnion :: String
lUnion = "\\cup"

stUnion :: String
stUnion = "+"

chInter :: Char
chInter = '\x2229'

lInter :: String
lInter = "\\cap"

stInter :: String
stInter = "&"

stOps :: String
stOps = "operations"

unOps :: [UnOp]
unOps = [AO, OE, User, Roles True, Roles False, Sessions
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

stCmpOp :: CmpOp -> String
stCmpOp o = case o of
  Elem -> "in"
  Eq -> "="
  Le -> "<="
  Lt -> "<"
  Ge -> ">="
  Gt -> ">"
  Ne -> "/="

csCmpOp :: CmpOp -> String
csCmpOp o = case o of
  Elem -> "\x2208"
  Le -> "\x2264"
  Ge -> "\x2265"
  Ne -> "\x2260"
  _ -> stCmpOp o

lCmpOp :: CmpOp -> String
lCmpOp o = case o of
  Elem -> "\\in"
  Le -> "\\leq"
  Ge -> "\\geq"
  Ne -> "\\neq"
  _ -> stCmpOp o

chAnd :: Char
chAnd = '\x2227'

stAnd :: String
stAnd = "/\\"

lAnd :: String
lAnd = "\\land"

chImpl :: Char
chImpl = '\x21D2'

stImpl :: String
stImpl = "=>"

lImpl :: String
lImpl = "\\Rightarrow"

-- | list of unicode characters legal in statement
keySigns :: String
keySigns = [chAnd, chImpl, chUnion, chInter, chEmpty]
  ++ concatMap csCmpOp [Elem, Le, Ge, Ne]

-- | replacement for Control.Exception.assert that requires compiler options
assert :: String -> Bool -> a -> a
assert s b a = if b then a else error $ "assert: " ++ s

-- | OCL compliant class name
stSet :: SetType -> String
stSet = foldSetType ("SetOf" ++) show

-- | USE compliant and disambiguated names
useOp :: Maybe Base -> UnOp -> String
useOp t o = let u = map (\ c -> if c == '*' then '_' else c) $ stUnOp o
  in case o of
  User -> if t == Just S then u else "users"
  Roles _ -> case t of
      Just r -> map toLower (show r) ++ u
      _ -> u
  _ -> u

-- | currently unused type representation
stType :: Type -> String
stType t = case t of
  SetTy s -> stSet s
  NatTy -> "\x2115" -- N
  EmptySetTy -> "{}"
