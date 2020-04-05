module Rcl.Cli (cli) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find, isSuffixOf)
import qualified Data.Map as Map (empty)
import Data.Maybe (fromMaybe)
import Rcl.Ast (UserTypes, Stmt)
import Rcl.Data (Model, getUserTypes)
import Rcl.Eval (evalInput)
import Rcl.Interpret (interprets)
import Rcl.Model (initModel)
import Rcl.Parse (parser, parseFromFile, ParseError)
import Rcl.Print (render, pStmts, Form (Form), Format (..))
import Rcl.Read (readTypes, readModel, readMyFile)
import Rcl.Reduce (reduction)
import Rcl.ToOcl (ocl)
import Rcl.ToSoil (toSoil)
import Rcl.Type (typeErrors)
import System.Console.GetOpt
import System.FilePath ((</>), addExtension)

cli :: String -> [String] -> IO ()
cli prN args = case getOpt Permute options args of
      (os, n, []) -> let o = foldl (flip id) defaultOpts os in
        if help o then putStrLn $
          usageInfo ("usage: " ++ prN ++ " [options] <file>*") options
        else let
          rm = readModel
            $ map (\ f -> dir o </> addExtension (f o) "txt")
            [rhFile, uaFile, paFile, sessFile, setsFile]
          in case n of
        [] -> if null os then rm >>= evalInput [] . initModel else
          putStrLn "unexpected options without file arguments"
        _ -> do
          eith <- if onlyPrint o then return $ Right Map.empty else
            if getTypes o then fmap Right . readTypes $ dir o </> typesFile o
            else fmap Left rm
          mapM_ (processFile eith o) n
      (_, _, errs) -> mapM_ putStrLn errs

-- | describe all available options
options :: [OptDescr (Opts -> Opts)]
options =
    [ Option "h" ["help"]
      (NoArg $ \ o -> o {help = True})
      "show help message"
    , Option "f" ["format"]
      (ReqArg (\ f o -> o {format = f, pprint = True}) "<format>")
      "print to stdout using format LaTeX, Ascii or Unicode (default)"
    , Option "n" ["nopars"]
      (NoArg $ \ o -> o {parens = False, pprint = True})
      "pretty print without parentheses"
    , Option "c" ["check"] (NoArg $ \ o -> o {check = True})
      "report type checking errors"
    , Option "r" ["reduce"] (NoArg $ \ o -> o {reduce = True})
      "show reductions and check reconstruction"
    , Option "t" ["types"] (NoArg $ \ o -> o {getTypes = True})
      "read user defined types from file"
    , Option "u" ["use-file"]
      (ReqArg (\ f o -> o {useFile = f, toOcl = True}) "<file>")
      "include RBAC use file in output file"
    , Option "o" ["output-file"]
      (ReqArg (\ f o -> o {outFile = f, toOcl = True}) "<file>")
      "write use (and soil) to .use (and .soil) output file(s)"
    , Option "e" ["evaluate"] (NoArg $ \ o -> o {evaluate = True})
      "evaluate formulas"
    , Option "i" ["interactive"] (NoArg $ \ o -> o {prompt = True})
      "prompt for interactive input" ]

data Opts = Opts
  { parens :: Bool
  , format :: String
  , pprint :: Bool
  , check :: Bool
  , reduce :: Bool
  , evaluate :: Bool
  , getTypes :: Bool
  , toOcl :: Bool
  , prompt :: Bool
  , help :: Bool
  , dir :: String
  , typesFile :: String
  , rhFile :: String
  , uaFile :: String
  , paFile :: String
  , sessFile :: String
  , setsFile :: String
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
  , getTypes = False
  , toOcl = False
  , prompt = False
  , help = False
  , dir = "examples"
  , typesFile = "types.txt"
  , rhFile = "rh"
  , uaFile = "ua"
  , paFile = "pa"
  , sessFile = "s"
  , setsFile = "sets"
  , useFile = "use/RBAC.use"
  , outFile = "use/test" }

onlyPrint :: Opts -> Bool
onlyPrint o = not $ or [check o, reduce o, evaluate o, toOcl o, prompt o]

form :: Opts -> Form
form o = let low = map toLower in
  Form (fromMaybe Uni $ find (\ f -> low (show f) == low (format o))
    [LaTeX, Ascii]) $ parens o

processFile :: Either Model UserTypes -> Opts -> String -> IO ()
processFile eith o file = parseFromFile parser file >>= reportParse eith o

reportParse :: Either Model UserTypes -> Opts -> Either ParseError [Stmt]
  -> IO ()
reportParse mus o eith = case eith of
  Left err -> print err
  Right ast -> do
    let p = pprint o
        c = check o
        r = reduce o
        t = toOcl o
        e = evaluate o
        i = prompt o
        out = outFile o
        suf = ".use"
        use = if suf `isSuffixOf` out then out else out ++ suf
        us = either getUserTypes id mus
    when (p || onlyPrint o) . putStrLn . render $ pStmts (form o) ast
    when c . putStrLn $ typeErrors us ast
    when r . putStrLn $ reduction us ast
    when t $ do
      str <- readMyFile (useFile o)
      writeFile use $ str ++ ocl us ast
      case mus of
        Left m -> writeFile (out ++ ".soil") $ toSoil m
        _ -> putStrLn "no .soil file written for option -t"
    case mus of
      Left m -> do
        let n = initModel m
        when e . putStrLn $ interprets us n ast
        when i $ evalInput ast n
      Right _ -> when (e || i) $ putStrLn
        "options -e or -i are incompatible with -t"
