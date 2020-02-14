module Rcl.Ast where

import Data.Char
import Data.List

data Stmt = CmpOp CmpOp Set Set
  | BoolOp BoolOp Stmt Stmt deriving (Eq, Show)

data CmpOp = Elem | Eq | Le | Lt | Ge | Gt | Ne deriving (Eq, Show)

data BoolOp = And | Impl deriving (Eq, Show)

data Set = PrimSet { stPrim :: String } | EmptySet | Num Int | UnOp UnOp Set
  | BinOp BinOp Set Set | Var Int Type deriving (Eq, Show)

data BinOp = Union | Inter | Minus | Pair deriving (Eq, Show)

data UnOp = AO | OE | User | Roles | RolesStar | Sessions
  | Permissions | PermissionsStar | Operations | Object | Card
  deriving (Eq, Show)
-- AO: all other, OE: one element

data SetType = ElemTy String | Set SetType | PairTy SetType SetType
  deriving (Eq, Show)

data Type = SetTy SetType | NatTy | EmptySetTy | Error deriving (Eq, Show)

chEmpty :: Char
chEmpty = '\x2205'

lEmpty :: String
lEmpty = "\\emptyset"

stEmpty :: String
stEmpty = "{}"

primTypes :: [String]
primTypes = ["U", "R", "OP", "OBJ", "P", "S"]

primSets :: [Set]
primSets = map PrimSet $ primTypes ++ ["CR", "CU", "CP"]

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
unOps = [AO, OE, User, RolesStar, Roles, Sessions
  , PermissionsStar, Permissions, Operations, Object]

stUnOp :: UnOp -> String
stUnOp o = let
  s = show o
  l = length s
  v = map toLower s in
  if l == 2 then s else
  if "Star" `isSuffixOf` s then take (l - 4) v ++ "*"
  else v

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
