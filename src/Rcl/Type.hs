module Rcl.Type (addPrimTypes, elemType, isElem, typeErrors, typeOf, typeSet,
                 wellTyped) where

import Control.Monad (foldM, unless, when)
import Control.Monad.State (State, modify, runState)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Rcl.Ast
import Rcl.Print (ppSet, ppTerm, ppStmt, ppStmts)

addPrimTypes :: UserTypes -> UserTypes
addPrimTypes = flip (foldr $ \ (b, s) -> Map.insertWith Set.union s
  . Set.singleton $ toSet b) primTypes

typeErrors :: UserTypes -> [Let] -> String
typeErrors us = unlines . concatMap
  (\ l -> let (es, ml) = wellTyped us l in
      case ml of
        Just t -> es ++ [ppStmts [t]]
        _ -> es)

typeSet :: UserTypes -> Set -> ([String], Maybe Set)
typeSet us s = case runState (tySet us s >>=
    filterType ("set: " ++ ppSet s) True (const True)) [] of
  (t, []) -> ([], Just t)
  (_, l) -> (reverse l, Nothing)

shadow :: String -- only a warning, no type error
shadow = "shadowing: "

wellTyped :: UserTypes -> Let -> ([String], Maybe Let)
wellTyped us s = let (t, e) = runState (tyLet us s) [] in
  (if null e then [] else reverse $ ("  in: " ++ ppStmts [s]) : e
  , if not $ any (not . isPrefixOf shadow) e then Just t else Nothing)

report :: String -> State [String] ()
report t = modify (t :)

tyLet :: UserTypes -> Let -> State [String] Let
tyLet us (Let as s) = do
  (bs, vs) <- foldM (\ (l, ws) (n, a) -> do
     b <- tySet ws a
     c <- filterType (n ++ " = " ++ ppSet b) True (const True) b
     case Set.toList $ typeOf c of
       [t] -> case Map.lookup n ws of
         Just ts -> if Set.member t ts then do
             report $ shadow ++ n ++ ":" ++ stSet t
             return ((n, c) : l, ws)
           else return ((n, c) : l, Map.insert n (Set.insert t ts) ws)
         Nothing -> return ((n, c) : l, Map.insert n (Set.singleton t) ws)
       _ -> do
         report $ "wrong definition of: " ++ n
         return ((n, c) : l, ws)) ([], us) as
  st <- tyStmt vs s
  return $ Let (reverse bs) st

tyStmt :: UserTypes -> Stmt -> State [String] Stmt
tyStmt = foldStmt FoldStmt
  { foldBool = \ _ o s1 s2 -> do
    r1 <- s1
    BoolOp o r1 <$> s2
  , foldCmp = \ _ o e1 e2 -> do
    t1 <- e1
    t2 <- e2
    let r = CmpOp o t1 t2
        str = ppStmt r
        md m = report $ m ++ ": " ++ str
        ft = filterType ("relation: " ++ str) True
    case (t1, t2) of
        (Term TheSet s1, Term TheSet s2) | o `elem` [Eq, Ne, Elem] -> do
            let ts1 = typeOf s1
                ts2 = typeOf s2
                ts = if o == Elem then
                  Set.filter (ifSet (`Set.member` ts1) False) ts2
                  else compatSetTys ts1 ts2
                filt t = any (`Set.member` ts) [t, SetOf t]
                filt0 f = if Set.null ts then const True else f
                filt1 t = if o == Elem then SetOf t `Set.member` ts else filt t
                filt2 t = if o == Elem then t `Set.member` ts else filt t
                mkSing = if o /= Elem then mkSingle ts else id
            b1 <- ft (filt0 filt1) s1
            b2 <- ft (filt0 filt2) s2
            when (Set.null ts) $ md "wrongly typed relation"
            pure . CmpOp o (Term TheSet $ mkSing b1) . Term TheSet $ mkSing b2
        (Term TheSet s1, EmptySet) | o `elem` [Eq, Ne] -> do
          n <- ft (not . isElem) s1
          pure $ CmpOp o (Term TheSet n) t2
        (Term Card _, Term Card _) | o /= Elem -> pure r
        (Term Card _, Num _) | o /= Elem -> pure r
        (Num _, Term Card _) | o /= Elem -> pure r
        _ -> do
          md "wrong comparison"
          pure r } . tyTerm

tyTerm :: UserTypes -> Term -> State [String] Term
tyTerm us t = case t of
  Term b s -> do
    r <- tySet us s
    case b of
      Card -> do
        n <- filterType ("cardinality: " ++ ppTerm t) True (not . isElem) r
        pure $ Term b n
      TheSet -> pure $ Term b r
  _ -> pure t

disambig :: String -> SetType -> Set -> State [String] Set
disambig str t s = let
  st = Set.singleton t
  rt = mkTypedSet st
  r = rt s
  filt = filterType str True
  ft = filt (\ e -> t `elem` [e, SetOf e])
  in case s of
  BinOp o s1 s2 -> case o of
    Operations _ -> pure r
    _ -> do
      m1 <- ft s1
      m2 <- ft s2
      pure . rt . BinOp o (mkSingle st m1) $ mkSingle st m2
  UnOp o s1 -> if isOp o then pure r else do
      r1 <- filt (\ t1 -> case o of
        OE -> SetOf t == t1
        AO -> t1 == t
        Typed _ ts -> t1 == t && Set.member t ts
        _ -> False) s1
      pure . rt $ UnOp o r1
  Braced bs -> do
    ms <- mapM (filterType str True ((== t) . SetOf)) bs
    pure . rt $ Braced ms
  _ -> pure r

filterType :: String -> Bool -> (SetType -> Bool) -> Set -> State [String] Set
filterType str b f s = let
  md m = report $ m ++ " in " ++ str
  t = typeOf s
  l = Set.toList t
  r = filter f l
  rs = Set.fromList r
  mt = mkTypedSet rs
  in case r of
  [] -> do
    unless (null l) . md $ "wrongly typed '" ++ ppSet s ++ "'"
    pure $ untyped s
  [a] -> case l of
    _ : _ : _ -> disambig str a $ untyped s
    _ -> pure s
  _ -> do
    when b . md $ "ambiguous '" ++ ppSet s ++ ":" ++ ppType rs ++ "'"
    pure . mt $ untyped s

tySet :: UserTypes -> Set -> State [String] Set
tySet us = let
  md t s = report $ t ++ ": " ++ ppSet s
  in foldSet FoldSet
   { foldBin = \ s o s1 s2 -> do
      a1 <- s1
      a2 <- s2
      case o of
        Operations b -> do
          b1 <- filterType ("1st arg: " ++ ppSet s)
            True (\ t -> b /= Star && isElemOrSet P t ||
                   any (`isElemOrSet` t) [R, U]) a1
          b2 <- filterType ("2nd arg: " ++ ppSet s) True (isElemOrSet OBJ) a2
          pure . mkTypedSet (Set.singleton $ toSet OP)
            . BinOp o (mkSingleton True b1) $ mkSingleton True b2
            -- R + U x OBJ -> 2^OP
        _ -> do
          let ts = Set.map (\ t -> if isElem t then SetOf t else t)
                . compatSetTys (typeOf a1) $ typeOf a2
              filt t = any (`Set.member` ts) [t, SetOf t]
              ft = filterType ("set operation: " ++ ppSet s) (Set.size ts <= 1)
                (if Set.null ts then const True else filt)
          b1 <- ft a1
          b2 <- ft a2
          when (Set.null ts) $ md "wrongly typed set operation" s
          pure . mkTypedSet ts . BinOp o (mkSingle ts b1) $ mkSingle ts b2
  , foldUn = \ s o s1 -> do
      a1 <- s1
      b1 <- filterType ("application: " ++ ppSet s) (isOp o) (opArg o) a1
      pure $ case o of
        Typed ex _ -> UnOp (Typed ex $ typeOf b1) $ untyped b1
        _ -> opResult o b1
  , foldBraced = \ s bs -> do
      ss <- sequence bs
      let ts = map typeOf ss
          ft = foldr1 Set.intersection ts
      fts <- mapM (filterType ("braced set: " ++ ppSet s) (Set.size ft <= 1)
        $ if Set.null ft then const True else (`Set.member` ft)) ss
      when (Set.null ft) $ md "wrongly typed braced set" s
      pure . mkTypedSet (Set.map SetOf ft) $ Braced fts
  , foldPrim = \ s -> case s of
      PrimSet p -> do
        let ts = Map.findWithDefault Set.empty p us
        when (Set.null ts) $ md "unknown base set" s
        pure $ mkTypedSet ts s
      _ -> pure s }

isOp :: UnOp -> Bool
isOp o = case o of
  OE -> False
  AO -> False
  Typed _ _ -> False
  _ -> True

mkTypedSet :: Set.Set SetType -> Set -> Set
mkTypedSet ts = if Set.null ts then id else UnOp $ Typed Derived ts

compatSetTys :: Set.Set SetType -> Set.Set SetType -> Set.Set SetType
compatSetTys s1 s2 = case Set.toList s1 of
  [] -> s2
  l1 -> case Set.toList s2 of
    [] -> s1
    l2 -> Set.unions [compatSetTysAux t1 t2 | t1 <- l1, t2 <- l2]

compatSetTysAux :: SetType -> SetType -> Set.Set SetType
compatSetTysAux t1 t2 = case (t1, t2) of
  (SetOf s1, _) | s1 == t2 -> Set.singleton t1
  (_, SetOf s2) | t1 == s2 -> Set.singleton t2
  _ -> if t1 == t2 then Set.singleton t1 else Set.empty

opArg :: UnOp -> SetType -> Bool
opArg o t = case o of
  User _ b -> b /= Star && t == ElemTy S || isElemOrSet R t
  Roles _ -> any (`isElemOrSet` t) [U, P, S]
  Sessions -> isElemOrSet U t
  Permissions _ -> any (`isElemOrSet` t) [R, U, S]
  Object _ -> isElemOrSet P t
  Iors _ _ -> isElemOrSet R t
  Typed _ ts -> Set.member t ts
  _ -> not $ isElem t

opResult :: UnOp -> Set -> Set
opResult o s = let
  ts = typeOf s
  mkSetType = Set.singleton . toSet
  in mkTypedSet (case o of
  OE -> elemType ts
  AO -> ts
  Typed _ _ -> ts
  User _ _ -> if ElemTy S `Set.member` ts then Set.singleton $ ElemTy U
    else mkSetType U
  Roles _ -> mkSetType R
  Sessions -> mkSetType S
  Permissions _ -> mkSetType P
  Object _ -> mkSetType OBJ
  Iors _ _ -> mkSetType R) . UnOp o $ mkSingleton (case o of
    User _ _ -> ElemTy S `Set.notMember` ts
    _ -> isOp o) s

mkSingle :: Set.Set SetType -> Set -> Set
mkSingle ts s =
  mkBraced (Set.size ts > 0 && Set.map SetOf (typeOf s) == ts) s

mkSingleton :: Bool -> Set -> Set
mkSingleton b s = let ts = typeOf s in
  mkBraced (b && Set.size ts == 1 && isElem (Set.findMin ts)) s

mkBraced :: Bool -> Set -> Set
mkBraced b s =
  if b then mkTypedSet (Set.map SetOf $ typeOf s) $ Braced [s] else s

isElemOrSet :: Base -> SetType -> Bool
isElemOrSet b t = case t of
  ElemTy i -> b == i
  SetOf (ElemTy i) -> b == i -- p 215 "general notation device"
  _ -> False

elemType :: Set.Set SetType -> Set.Set SetType
elemType = Set.foldr (ifSet Set.insert id) Set.empty

ifSet :: (SetType -> a) -> a -> SetType -> a
ifSet f c s = case s of
  SetOf e -> f e
  _ -> c

isElem :: SetType -> Bool
isElem = ifSet (const False) True
