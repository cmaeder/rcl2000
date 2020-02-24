module Rcl.Parse (parser, parseFromFile, ParseError) where

import Data.Char (isLetter)
import Data.Functor (void)
import Rcl.Ast
import Text.ParserCombinators.Parsec

parser :: Parser [Stmt]
parser = skip *> many1 stmt <* eof

stmt :: Parser Stmt
stmt = mayBe (BoolOp And)
  <$> impl <*> optionMaybe (alts [stAnd, [chAnd], lAnd, "^"] *> stmt)

impl :: Parser Stmt
impl = mayBe (BoolOp Impl)
  <$> cmp <*> optionMaybe (alts [stImpl, [chImpl], lImpl] *> cmp)

cmp :: Parser Stmt
cmp = cmpSet <|> cmpCard

cmpCard :: Parser Stmt
cmpCard = flip CmpOp <$> pCardTerm <*> cmpOp <*> (pCardTerm <|> nat)

pCardTerm :: Parser Term
pCardTerm = Term True <$> (bar *> set <* bar)

bar :: Parser Char
bar = pch '|'

-- allow leading zero
nat :: Parser Term
nat = Num . read <$> many1 digit <* skip

cmpSet :: Parser Stmt
cmpSet = do
  t <- pTerm
  o <- setCmpOp
  CmpOp o t <$> case o of
    Elem -> pTerm
    _ -> emptySet <|> pTerm

pTerm :: Parser Term
pTerm = Term False <$> set

emptySet :: Parser Term
emptySet = EmptySet <$ alts [stEmpty, [chEmpty], lEmpty]

setCmpOp :: Parser CmpOp
setCmpOp = let
  lOps = [Elem, Ne]
  aOps = Eq : lOps
  in choice $ map (pString stCmpOp) aOps
  ++ map (pString csCmpOp) lOps
  ++ map (pString lCmpOp) lOps

cmpOp :: Parser CmpOp
cmpOp = let
  cmpOps = [Eq, Le, Lt, Ge, Gt, Ne]
  altCmpOps = [Le, Ge, Ne]
  in choice $ map (pString stCmpOp) cmpOps
  ++ map (pString csCmpOp) altCmpOps
  ++ map (pString lCmpOp) altCmpOps

set :: Parser Set
set = mayBe (BinOp Union) <$> interSet <*> optionMaybe (uOp *> set)

uOp :: Parser String
uOp = alts [stUnion, [chUnion], lUnion]

interSet :: Parser Set
interSet = mayBe (BinOp Inter)
  <$> applSet <*> optionMaybe (iOp *> interSet)

iOp :: Parser String
iOp = alts [stInter, [chInter], lInter]

mayBe :: (a -> a -> a) -> a -> Maybe a -> a
mayBe f a = maybe a $ f a

applSet :: Parser Set
applSet = unOpSet <|> opsSet <|> primSet

opsSet :: Parser Set
opsSet = BinOp Ops <$> (pString (const stOps) Ops
  *> pch '(' *> set <* pch ',') <*> set <* pch ')'

primSet :: Parser Set
primSet = (PrimSet <$> many1 letter <* skip) <|> parenSet

unOpSet :: Parser Set
unOpSet = UnOp <$> choice pUnOps <*> applSet

pUnOps :: [Parser UnOp]
pUnOps = map (pString lUnOp) [Roles True, Permissions True]
  ++ map (pString stUnOp) unOps

parenSet :: Parser Set
parenSet = pch '(' *> set <* pch ')'

pch :: Char -> Parser Char
pch c = char c <* skip

alts :: [String] -> GenParser Char () String
alts = choice . map (pString id)

pString :: (a -> String) -> a -> Parser a
pString pr a = let s = pr a in
  a <$ try (string s <* notFollowedBy
       (case s of
         _ : _ | isLetter $ last s -> alphaNum
         _ -> oneOf $ "&*+-/<=>" ++ keySigns)
     <* skip)

skip :: Parser ()
skip = skipMany $ void space <|> nestedComment "/*" "*/" <|> lineComment "//"
  <|> void (oneOf "$~")

-- | nested comments, open and closing strings must have at least one char
nestedComment :: String -> String -> Parser ()
nestedComment op cl =
  let inComment = void (try $ string cl)
        <|> (nestedComment op cl <|> void anyChar) *> inComment
  in try (string op) *> inComment

-- | line comments
lineComment :: String -> Parser ()
lineComment op =
  try (string op) *> void (manyTill anyChar $ void newline <|> eof)
