module Rcl.ToOcl where

import Rcl.Ast
import Rcl.Fold
import Rcl.Print (pSet, form)
import Rcl.Reduce (runReduce, Vars)
import Rcl.Type (typeOfSet, isElem)
import Text.PrettyPrint

ocl :: [Stmt] -> String
ocl = unlines . map (\ (n, s) -> render $ hcat
    [ text $ "inv i" ++ show n ++ ": "
    , uncurry toOcl $ runReduce s]) . zip [1 :: Int ..]

toOcl :: Stmt -> Vars -> Doc
toOcl = foldl (\ f (i, s) -> cat
   [ hcat [setToOcl s, arr, text "forAll"]
   , parens $ sep [text $ 'v' : show i ++ " |", f]]) . stmtToOcl

arr :: Doc
arr = text "->"

stmtToOcl :: Stmt -> Doc
stmtToOcl = foldStmt FoldStmt
  { foldBool = \ (BoolOp _ s1 s2) o d1 d2 ->
      sep [parenStmt s1 d1, pBoolOp o <+> parenStmt s2 d2]
  , foldCmp = \ (CmpOp _ _ s2) o d1 d2 -> case o of
      Elem -> cat [hcat [d2, arr, text "includes"], parens d1]
      Eq | s2 == EmptySet -> hcat [d1, arr, text "isEmpty()"]
      Ne | s2 == EmptySet -> hcat [d1, arr, text "notEmpty()"]
      _ -> sep [d1, pCmpOp o <+> d2] } setToOcl

parenStmt :: Stmt -> Doc -> Doc
parenStmt s = case s of
  BoolOp Impl _ _ -> parens
  _ -> id

setToOcl :: Set -> Doc
setToOcl = foldSet FoldSet
  { foldBin = \ _ o d1 d2 -> case o of
      Pair -> cat [hcat [d1, pBinOp o], d2]
      _ -> cat [hcat [d1, arr, pBinOp o], parens d2]
  , foldUn = \ (UnOp _ s) o d -> case o of
      Card -> hcat [d, arr, text "size()"]
      _ -> cat [pUnOp o, parens
        $ if isElem (typeOfSet s) then hcat [text "Set", braces d] else d]
  , foldPrim = pSet form }

pBoolOp :: BoolOp -> Doc
pBoolOp o = text $ case o of
  And -> "and"
  Impl -> "implies"

pCmpOp :: CmpOp -> Doc
pCmpOp o = text $ case o of
  Ne -> "<>"
  _ -> stCmpOp o

pBinOp :: BinOp -> Doc
pBinOp o = text $ case o of
  Union -> "union"
  Inter -> "intersection"
  Minus -> "excluding"
  Pair -> ","

pUnOp :: UnOp -> Doc
pUnOp = text . map (\ c -> if c == '*' then '_' else c) . stUnOp
