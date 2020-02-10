module Rcl.Ast where

import Data.Char
import Data.List

data Set = U | R | OP | OBJ | P | S | CR | CU | CP | EmptySet | Num Int
  | UnOp UnOp Set | BinOp BinOp Set Set deriving Show

chEmpty :: Char
chEmpty = '\x2205'

primSets :: [Set]
primSets = [U, R, OP, OBJ, P, S, CR, CU, CP]

data BinOp = Union | Inter deriving Show

chUnion :: Char
chUnion = '\x222A'

chInter :: Char
chInter = '\x2229'

data UnOp = AO | OE | User | Roles | RolesStar | Sessions
  | Permissions | PermissionsStar | Operations | Object | Card deriving Show
-- AO: all other, OE: one element

unOps :: [UnOp]
unOps = [AO, OE, User, RolesStar, Roles, Sessions
  , PermissionsStar, Permissions, Operations, Object]

prUnOp :: UnOp -> String
prUnOp o = let
  s = show o
  l = length s
  v = map toLower s in
  if l == 2 then s else
  if "Star" `isSuffixOf` s then take (l - 4) v ++ "*"
  else v

data Stmt = CmpOp CmpOp Set Set
  | BoolOp BoolOp Stmt Stmt deriving Show

data CmpOp = Elem | Eq | Le | Lt | Ge | Gt | Ne deriving Show

cmpOps :: [CmpOp]
cmpOps = [Elem, Eq, Le, Lt, Ge, Gt, Ne]

prCmpOp :: CmpOp -> String
prCmpOp o = case o of
  Elem -> "in"
  Eq -> "="
  Le -> "<="
  Lt -> "<"
  Ge -> ">="
  Gt -> ">"
  Ne -> "/="

altCmpOps :: [CmpOp]
altCmpOps = [Elem, Le, Ge, Ne]

prAltCmpOp :: CmpOp -> String
prAltCmpOp o = case o of
  Elem -> "\x2208"
  Le -> "\x2264"
  Ge -> "\x2265"
  Ne -> "\x2260"
  _ -> prCmpOp o

data BoolOp = And | Impl deriving Show

chAnd :: Char
chAnd = '\x2227'

chImpl :: Char
chImpl = '\x21D2'

keySigns :: String
keySigns = [chAnd, chImpl, chUnion, chInter, chEmpty]
  ++ concatMap prAltCmpOp altCmpOps
