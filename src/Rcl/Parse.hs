module Rcl.Parse (set, parser, parseFromFile, ParseError) where

import Data.Char (isLetter)
import Data.Functor (void)
import Rcl.Ast
import Text.ParserCombinators.Parsec

parser :: Parser [Stmt]
parser = skip *> many1 stmt <* eof

stmt :: Parser Stmt
stmt = mayBe (BoolOp And)
  <$> impl <*> optionMaybe (alts sAnd *> stmt)

impl :: Parser Stmt
impl = mayBe (BoolOp Impl)
  <$> cmp <*> optionMaybe (alts sImpl *> cmp)

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
emptySet = EmptySet <$ alts sEmpty

setCmpOp :: Parser CmpOp
setCmpOp = let lOps = [Elem, Ne] in choice $ pString (sCmpOp Ascii) Eq
    : concatMap (\ f -> map (pString $ sCmpOp f) lOps) forms

cmpOp :: Parser CmpOp
cmpOp = let cmpOps = [Le, Ge, Ne] in choice
  $ map (pString $ sCmpOp Ascii) [Eq, Lt, Gt]
  ++ concatMap (\ f -> map (pString $ sCmpOp f) cmpOps) forms

set :: Parser Set
set = mayBe (BinOp Union) <$> interSet <*> optionMaybe (alts sUnion *> set)

interSet :: Parser Set
interSet = mayBe (BinOp Inter)
  <$> minusSet <*> optionMaybe (alts sInter *> interSet)

mayBe :: (a -> a -> a) -> a -> Maybe a -> a
mayBe f a = maybe a $ f a

minusSet :: Parser Set
minusSet = mayBe (BinOp Minus)
  <$> applSet <*> optionMaybe (pch '-' *> (braceSet <|> primSet))

braceSet :: Parser Set
braceSet = pch '{' *> set <* pch '}'

applSet :: Parser Set
applSet = unOpSet <|> opsSet <|> primSet

opsSet :: Parser Set
opsSet = BinOp Ops <$> (pString (const stOps) Ops
  *> pch '(' *> set <* pch ',') <*> set <* pch ')'

primSet :: Parser Set
primSet = (PrimSet <$> setName <* skip) <|> parenSet

setName :: Parser String
setName = (:) <$> letter <*> many (alphaNum <|> char '_')

unOpSet :: Parser Set
unOpSet = UnOp <$> choice pUnOps <*> applSet

pUnOps :: [Parser UnOp]
pUnOps = map (pString lUnOp) [Roles True, Permissions True]
  ++ map (pString stUnOp) unOps

parenSet :: Parser Set
parenSet = pch '(' *> set <* pch ')'

pch :: Char -> Parser Char
pch c = char c <* skip

alts :: (Format -> String) -> GenParser Char () String
alts f = choice $ map (pString id . f) forms

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
