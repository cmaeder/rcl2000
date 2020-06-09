module Rcl.Parse (pType, parser, set, setDef, stmt) where

import Data.Char (isLetter)
import Data.Functor (void)
import qualified Data.Set as Set (singleton)

import Rcl.Ast

import Text.ParserCombinators.Parsec

parser :: Parser [Stmt]
parser = skip *> many stmt <* eof

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
pCardTerm = Term Card <$> (bar *> set <* bar)

bar :: Parser Char
bar = pch '|'

-- allow leading zero
nat :: Parser Term
nat = do
  ds <- (try (string "0" <* notFollowedBy digit) <|>
    skipMany (char '0') *> many1 digit) <* skip
  if length ds > 9 then unexpected "digits: number may become too big" else
    return . Num $ read ds

cmpSet :: Parser Stmt
cmpSet = do
  t <- pTerm
  o <- setCmpOp
  CmpOp o t <$> case o of
    Elem -> pTerm
    _ -> emptySet <|> pTerm

pTerm :: Parser Term
pTerm = Term TheSet <$> set

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
  <$> applSet <*> optionMaybe (pch '-' *> typedSet)

bracedSet :: Parser Set
bracedSet = Braced <$> braced sets

braced :: Parser a -> Parser a
braced p = pch '{' *> p <* pch '}'

sets :: Parser [Set]
sets = list set

list :: Parser a -> Parser [a]
list p = (:) <$> p <*> many (optional (pch ',') *> p)

applSet :: Parser Set
applSet = unOpSet <|> opsSet <|> typedSet

opsSet :: Parser Set
opsSet = do
  o <- pStar Operations
  let p = BinOp o <$> (pch '(' *> set <* pch ',') <*> set <* pch ')'
  if o == Operations TheOp then p <|> return (PrimSet $ stUnOp o) else p

typedSet :: Parser Set
typedSet = (\ p -> maybe p (\ t -> UnOp (Typed Explicit $ Set.singleton t) p))
  <$> primSet <*> mType

typedName :: Parser (String, Maybe SetType)
typedName = curry id <$> setName <*> mType

setDef :: Parser [(String, Maybe SetType)]
setDef = let l = list typedName in
  skip *> (((:) <$> (typedName <* optional (pch '=')) <*> (l <|> braced l))
  <|> return []) <* eof

mType :: Parser (Maybe SetType)
mType = optionMaybe (pch ':' *> pType <* skip)

primSet :: Parser Set
primSet = PrimSet <$> setName <|> parenSet <|> bracedSet

setName :: Parser String
setName = (:) <$> letter <*> (many (alphaNum <|> char '_') <* skip)

pType :: Parser SetType
pType = foldr (const SetOf) . ElemTy
  <$> choice (map (\ (a, s) -> try (string s) >> return a) primTypes)
  <*> many (char 's')

unOpSet :: Parser Set
unOpSet = do
  u <- choice pUnOps
  let p = UnOp u <$> applSet
  if u `elem` map ($ Star) stars then p else p <|> return (PrimSet $ stUnOp u)

pUnOps :: [Parser UnOp]
pUnOps = map pStar stars
  ++ map (pString stUnOp) unOps

pStar :: (Show a) => (OptStar -> a) -> Parser a
pStar p = try (string . stUnOp $ p TheOp) *>
  (p Star <$ (string "*" <|> try (string "^{*}"))
    <|> p TheOp <$ notFollowedBy alphaNum) <* skip

unOps :: [UnOp]
unOps = [AO, OE, Sessions, Object Plural, Object Singular]

stars :: [OptStar -> UnOp]
stars = [User Plural, User Singular, Roles, Permissions, Iors Jun, Iors Sen]

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
