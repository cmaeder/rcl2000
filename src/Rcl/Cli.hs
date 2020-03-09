{-# LANGUAGE TupleSections #-}
module Rcl.Cli (cli) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find, isSuffixOf)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)
import Rcl.Ast (UserTypes, SetType (..), Base (..), Stmt)
import Rcl.Interpret (interprets, getUserTypes)
import Rcl.Model (initModel)
import Rcl.Parse (parser, parseFromFile, ParseError)
import Rcl.Print (render, pStmts, Form (Form), Format (..))
import Rcl.Read (readModel)
import Rcl.Reduce (reduction)
import Rcl.ToOcl (ocl)
import Rcl.ToSoil (toSoil)
import Rcl.Type (typeErrors)
import System.Console.GetOpt

userTypes :: UserTypes
userTypes = fromList $ concatMap (\ (l, t) -> map (, t) l)
  [(["CU"], Set . Set $ ElemTy U)
  , (["CP"], Set . Set $ ElemTy P)
  , (["CR", "read", "write", "AR", "ASR", "SR"], Set . Set $ ElemTy R)
  , (["GR"], Set . Set . Set $ ElemTy R)
  , (["RR", "WR", "OWN", "PARENTGRANT", "PARENT", "READ"], Set $ ElemTy R)
  , (["wp", "rp", "OWNAPM", "OWNRPM", "PGPM", "PPM", "RPM"], Set $ ElemTy P)]

cli :: String -> [String] -> IO ()
cli prN args = case args of
    [] -> putStrLn $
      usageInfo ("usage: " ++ prN ++ " [options] <file>+") options
    _ -> case getOpt Permute options args of
      (o, n, []) -> case n of
        [] -> putStrLn "missing file arguments"
        _ -> mapM_ (processFile userTypes $ foldl (flip id) defaultOpts o) n
      (_, _, errs) -> mapM_ putStrLn errs

-- | describe all available options
options :: [OptDescr (Opts -> Opts)]
options =
    [ Option "n" ["nopars"]
      (NoArg $ \ o -> o {parens = False, pprint = True})
      "pretty print without parentheses"
    , Option "p" ["print"] (NoArg $ \ o -> o {pprint = True})
      "pretty print"
    , Option "c" ["check"] (NoArg $ \ o -> o {check = True})
      "type check"
    , Option "r" ["reduce"] (NoArg $ \ o -> o {reduce = True})
      "reduce and reconstruct"
    , Option "e" ["evaluate"] (NoArg $ \ o -> o {evaluate = True})
      "evaluate formulas"
    , Option "f" ["format"]
      (ReqArg (\ f o -> o {format = f, pprint = True}) "<format>")
      "use format LaTeX, Ascii or Unicode (default)"
    , Option "u" ["use-file"]
      (ReqArg (\ f o -> o {useFile = f, toOcl = True}) "<file>")
      "include use file <file>"
    , Option "o" ["output-file"]
      (ReqArg (\ f o -> o {outFile = f, toOcl = True}) "<file>")
      "write output to file <file>" ]

data Opts = Opts
  { parens :: Bool
  , format :: String
  , pprint :: Bool
  , check :: Bool
  , reduce :: Bool
  , evaluate :: Bool
  , toOcl :: Bool
  , useFile :: String
  , outFile :: String }

defaultOpts :: Opts
defaultOpts = Opts
  { parens = True
  , format = "Uni"
  , pprint = False
  , check = False
  , reduce = False
  , evaluate = False
  , toOcl = False
  , useFile = "use/RBAC.use"
  , outFile = "use/test" }

form :: Opts -> Form
form o = let low = map toLower in
  Form (fromMaybe Uni $ find (\ f -> low (show f) == low (format o))
    [LaTeX, Ascii]) $ parens o

processFile :: UserTypes -> Opts -> String -> IO ()
processFile us o file = parseFromFile parser file >>= reportParse us o

reportParse :: UserTypes -> Opts -> Either ParseError [Stmt] -> IO ()
reportParse us o eith = case eith of
  Left err -> print err
  Right ast -> do
    let p = pprint o
        c = check o
        r = reduce o
        i = toOcl o
        e = evaluate o
        a = all (== False) [p, c, r, i, e]
        out = outFile o
        suf = ".use"
        use = if suf `isSuffixOf` out then out else out ++ suf
    when (p || a) . putStrLn . render $ pStmts (form o) ast
    when (c || a) . putStrLn $ typeErrors us ast
    when (r || a) . putStrLn $ reduction us ast
    when (i && not e) $ do
      str <- readFile (useFile o)
      let cont = str ++ ocl us ast
      writeFile use cont
    when e $ do
      m <- readModel
      let uts = getUserTypes m
      if i then do
          str <- readFile (useFile o)
          writeFile use $ str ++ ocl uts ast
          writeFile (out ++ ".soil") $ toSoil m
        else putStrLn $ interprets uts (initModel m) ast
