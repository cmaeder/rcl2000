module Rcl.Ast where

import Data.Char (isLetter, toLower)
import Data.List (isSuffixOf)

data Stmt = CmpOp CmpOp Set Set
  | BoolOp BoolOp Stmt Stmt deriving (Eq, Show)

data CmpOp = Elem | Eq | Le | Lt | Ge | Gt | Ne deriving (Eq, Show)

data BoolOp = And | Impl deriving (Eq, Show)

data Set = PrimSet { stPrim :: String } | EmptySet | Num Int | UnOp UnOp Set
  | BinOp BinOp Set Set | Var Var deriving (Eq, Show)

data Var = MkVar Int String Type deriving (Eq, Show)

data BinOp = Union | Inter | Minus | Pair deriving (Eq, Show)

data UnOp = AO | OE | Card | User | Roles Bool | Sessions
  | Permissions Bool | Operations | Objects deriving (Eq, Show)
-- AO: all other, OE: one element, object ~> objects, Bool for * suffix

data Base = U | R | OP | OBJ | P | S deriving (Eq, Ord, Show)

data SetType = ElemTy Base | Set SetType | PairTy SetType SetType
  deriving (Eq, Show)

data Type = SetTy SetType | NatTy | EmptySetTy | Error deriving (Eq, Show)

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

unOps :: [UnOp]
unOps = [AO, OE, User, Roles True, Roles False, Sessions
  , Permissions True, Permissions False, Operations, Objects]

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

keySigns :: String
keySigns = [chAnd, chImpl, chUnion, chInter, chEmpty]
  ++ concatMap csCmpOp [Elem, Le, Ge, Ne]
