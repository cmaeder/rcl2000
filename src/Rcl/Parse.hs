module Rcl.Parse where

import Data.Char
import Rcl.Ast
import Text.ParserCombinators.Parsec

parser :: Parser Stmt
parser = mayBe (BoolOp And)
  <$> impl <*> optionMaybe (alts ["/\\", [chAnd]] *> parser)

impl :: Parser Stmt
impl = mayBe (BoolOp Impl)
  <$> cmp <*> optionMaybe (alts ["=>", [chImpl]] *> cmp)

alts :: [String] -> GenParser Char () String
alts = choice . map (pString id)

cmp :: Parser Stmt
cmp = flip CmpOp <$> set <*> cmpOp <*> set

cmpOp :: Parser CmpOp
cmpOp = choice (map (pString prCmpOp) cmpOps)
  <|> choice (map (pString prAltCmpOp) altCmpOps)

-- possibly a top-level union
set :: Parser Set
set = mayBe (BinOp Union)
  <$> interSet <*> optionMaybe (oneCh (chUnion : "u") *> set)

interSet :: Parser Set
interSet = mayBe (BinOp Inter)
  <$> applSet <*> optionMaybe (oneCh (chInter : "in") *> interSet)

mayBe :: (a -> a -> a) -> a -> Maybe a -> a
mayBe f a = maybe a $ f a

applSet :: Parser Set
applSet = primSet <|> cardSet <|> unOpSet

unOpSet :: Parser Set
unOpSet = UnOp <$> choice (map (pString prUnOp) unOps) <*> (primSet <|> unOpSet)

cardSet :: Parser Set
cardSet = pch '|' *> set <* pch '|'

parenSet :: Parser Set
parenSet = pch '(' *> set <* pch ')'

pch :: Char -> Parser Char
pch c = char c <* spaces

primSet :: Parser Set
primSet = intSet <* spaces <|> emptySet
  <|> choice (map (pString show) primSets)
  <|> parenSet

intSet :: Parser Set
intSet = Num . read <$> many1 digit

emptySet :: Parser Set
emptySet = EmptySet <$ oneCh (chEmpty : "e")

oneCh :: String -> Parser Char
oneCh = choice . map (pString (: ""))

pString :: (a -> String) -> a -> Parser a
pString pr a = let s = pr a in
  a <$ try (string s <* notFollowedBy
       (case s of
         _ : _ | isLetter $ last s -> alphaNum
         _ -> satisfy isSymbol)
     <* spaces)
