module Rcl.ToOcl where

import Rcl.Ast
import Rcl.Fold
import Rcl.Print (pSet, form)
import Rcl.Reduce (runReduce, Vars)
import Text.PrettyPrint

ocl :: [Stmt] -> String
ocl = unlines . map (render . uncurry toOcl . runReduce)

toOcl :: Stmt -> Vars -> Doc
toOcl = foldl (\ f (i, s) -> cat
   [ hcat [pSet form s, arr, text "forAll"]
   , parens $ sep [text $ 'v' : show i ++ " |", f]])
   . stmtToOcl

arr :: Doc
arr = text "->"

stmtToOcl :: Stmt -> Doc
stmtToOcl = foldStmt FoldStmt
  { foldBool = \ (BoolOp _ s1 s2) o d1 d2 ->
      sep [parenStmt s1 d1, pBoolOp o <+> parenStmt s2 d2]
  , foldCmp = \ _ o d1 d2 -> case o of
      Elem -> cat [hcat [d2, arr, text "includes"], parens d1]
      -- Ne -> cat [text "not", parens $ sep [d1, pCmpOp Eq <+> d2]]
      _ -> sep [d1, pCmpOp o <+> d2] } setToOcl

parenStmt :: Stmt -> Doc -> Doc
parenStmt s = case s of
  BoolOp Impl _ _ -> parens
  _ -> id

setToOcl :: Set -> Doc
setToOcl = foldSet FoldSet
  { foldBin = \ (BinOp _ s1 _) o d1 d2 -> case o of
      Pair -> parens $ cat [hcat [d1, pBinOp o], d2]
      Minus -> cat [parenSet s1 d1, cat [pBinOp o, text "Set", braces d2]]
      _ -> cat [hcat [parenSet s1 d1, arr, pBinOp o], parens d2]
  , foldUn = \ (UnOp _ s) o d -> case o of
      Card -> hcat [parenSet s d, arr, text "size()"]
      _ -> cat [pUnOp o, case s of
        BinOp Pair _ _ -> d
        _ -> parens d]
  , foldPrim = primToOcl }

parenSet :: Set -> Doc -> Doc
parenSet s = case s of
  BinOp o _ _ -> case o of
    Pair -> id
    _ -> parens
  _ -> id

primToOcl :: Set -> Doc
primToOcl s = case s of
  EmptySet -> text "Set{}"
  _ -> pSet form s

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
  Minus -> "-"
  Pair -> ","

pUnOp :: UnOp -> Doc
pUnOp = text . map (\ c -> if c == '*' then '_' else c) . stUnOp
