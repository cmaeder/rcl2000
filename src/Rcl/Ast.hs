module Rcl.Ast where

import Data.Char
import Data.List

data Set = U | R | OP | OBJ | P | S | CR | CU | CP | EmptySet | Num Int
  | UnOp UnOp Set | BinOp BinOp Set Set | Var Int deriving (Eq, Show)

chEmpty :: Char
chEmpty = '\x2205'

lEmpty :: String
lEmpty = "\\emptyset"

stEmpty :: String
stEmpty = "{}"

primSets :: [Set]
primSets = [U, R, OP, OBJ, P, S, CR, CU, CP]

data BinOp = Union | Inter | Minus deriving (Eq, Show)

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

data UnOp = AO | OE | User | Roles | RolesStar | Sessions
  | Permissions | PermissionsStar | Operations | Object | Card
  deriving (Eq, Show)
-- AO: all other, OE: one element

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

data Stmt = CmpOp CmpOp Set Set
  | BoolOp BoolOp Stmt Stmt deriving Show

data CmpOp = Elem | Eq | Le | Lt | Ge | Gt | Ne deriving (Eq, Show)

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

data BoolOp = And | Impl deriving Show

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
