module Main where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Rcl.Ast (Stmt)
import Rcl.Parse (parser, parseFromFile, ParseError)
import Rcl.Print (render, pStmts, Form (Form), Format (..))
import Rcl.Reduce (reduction)
import Rcl.ToOcl (ocl)
import Rcl.Type (typeErrors)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  prN <- getProgName
  case args of
    [] -> putStrLn $
      usageInfo ("usage: " ++ prN ++ " [options] <file>+") options
    _ -> case getOpt Permute options args of
      (o, n, []) -> case n of
        [] -> putStrLn "missing file arguments"
        _ -> mapM_ (processFile $ foldl (flip id) defaultOpts o) n
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
    , Option "i" ["invariants"] (NoArg $ \ o -> o {toOcl = True})
      "output ocl invariants"
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
  , toOcl = False
  , useFile = "use/RBAC.use"
  , outFile = "" }

form :: Opts -> Form
form o = let low = map toLower in
  Form (fromMaybe Uni $ find (\ f -> low (show f) == low (format o))
    [LaTeX, Ascii]) $ parens o

processFile :: Opts -> String -> IO ()
processFile o file = parseFromFile parser file >>= reportParse o

reportParse :: Opts -> Either ParseError [Stmt] -> IO ()
reportParse o eith = case eith of
  Left err -> print err
  Right ast -> do
    let p = pprint o
        c = check o
        r = reduce o
        i = toOcl o
        a = all (== False) [p, c, r, i]
    when (p || a) . putStrLn . render $ pStmts (form o) ast
    when (c || a) . putStrLn $ typeErrors ast
    when (r || a) . putStrLn $ reduction ast
    when i $ do
      str <- readFile (useFile o)
      let cont = str ++ ocl ast
      case outFile o of
        "" -> putStrLn cont
        out -> writeFile out cont
