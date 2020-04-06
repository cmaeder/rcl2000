module Rcl.Cli (cli) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find)
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
import System.FilePath ((</>), replaceExtension)

cli :: String -> [String] -> IO ()
cli prN args = case getOpt Permute options args of
      (os, n, []) -> let o = foldl (flip id) dOpts os in
        if help o then putStrLn $
          usageInfo ("usage: " ++ prN ++ " [options] <file>*") options
        else let
          rm = readModel $ map (optsFile o)
            [rhFile, uaFile, paFile, sessFile, setsFile]
          in case n of
        [] -> if null os then rm >>= evalInput [] . initModel else
          putStrLn "unexpected options without file arguments"
        _ -> do
          eith <- if onlyPrint o then return $ Right Map.empty else
            if getTypes o then fmap Right . readTypes $ optsFile o typesFile
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
    , Option "d" ["dir"]
      (ReqArg (\ f o -> o {dir = f}) "<dir>")
      $ "directory for input files, default: " ++ dir dOpts
    , Option "x" ["ext"]
      (ReqArg (\ f o -> o {ext = f}) "<ext>")
      $ "extension for input files, default: " ++ ext dOpts
    , Option "t" ["types"]
      (OptArg (\ mt o -> let o1 = o {getTypes = True} in
                  maybe o1 (\ t -> o1 {typesFile = t}) mt) "file")
      $ "read user defined types from file, default: " ++ typesFile dOpts
    , Option "R" ["Roles"]
      (ReqArg (\ f o -> o {rhFile = f}) "<file>")
      $ "file for roles and role hierarchy, default: " ++ rhFile dOpts
    , Option "U" ["Users"]
      (ReqArg (\ f o -> o {uaFile = f}) "<file>")
      $ "file for users to roles assignment, default: " ++ uaFile dOpts
    , Option "P" ["Perms"]
      (ReqArg (\ f o -> o {paFile = f}) "<file>")
      $ "file for permissions to roles assignment, default: " ++ paFile dOpts
    , Option "S" ["Sessions"]
      (ReqArg (\ f o -> o {sessFile = f}) "<file>")
      $ "file for sessions with user and roles, default: " ++ sessFile dOpts
    , Option "s" ["sets"]
      (ReqArg (\ f o -> o {setsFile = f}) "<file>")
      $ "file for user defined (conflict) sets, default: " ++ setsFile dOpts
    , Option "u" ["use-file"]
      (ReqArg (\ f o -> o {useFile = f, toOcl = True}) "<file>")
      $ "include RBAC use file in output file, default: " ++ useFile dOpts
    , Option "o" ["output-file"]
      (ReqArg (\ f o -> o {outFile = f, toOcl = True}) "<file>")
      $ "write use (and soil) to .use (and .soil) output file(s), default: "
      ++ outFile dOpts
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
  , dir :: FilePath
  , ext :: String
  , typesFile :: FilePath
  , rhFile :: FilePath
  , uaFile :: FilePath
  , paFile :: FilePath
  , sessFile :: FilePath
  , setsFile :: FilePath
  , useFile :: FilePath
  , outFile :: FilePath }

dOpts :: Opts
dOpts = Opts
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
  , ext = "txt"
  , typesFile = "types"
  , rhFile = "rh"
  , uaFile = "ua"
  , paFile = "pa"
  , sessFile = "s"
  , setsFile = "sets"
  , useFile = "use/RBAC.use"
  , outFile = "use/test" }

optsFile :: Opts -> (Opts -> FilePath) -> FilePath
optsFile o sel = dir o </> replaceExtension (sel o) (ext o)

onlyPrint :: Opts -> Bool
onlyPrint o = not $ or [check o, reduce o, evaluate o, toOcl o, prompt o]

form :: Opts -> Form
form o = let low = map toLower in
  Form (fromMaybe Uni $ find (\ f -> low (show f) == low (format o))
    [LaTeX, Ascii]) $ parens o

processFile :: Either Model UserTypes -> Opts -> FilePath -> IO ()
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
        use = replaceExtension out "use"
        us = either getUserTypes id mus
    when (p || onlyPrint o) . putStrLn . render $ pStmts (form o) ast
    when c . putStrLn $ typeErrors us ast
    when r . putStrLn $ reduction us ast
    when t $ do
      str <- readMyFile (useFile o)
      writeFile use $ str ++ ocl us ast
      case mus of
        Left m -> writeFile (replaceExtension out "soil") $ toSoil m
        _ -> putStrLn "no .soil file written for option -t"
    case mus of
      Left m -> do
        let n = initModel m
        when e . putStrLn $ interprets us n ast
        when i $ evalInput ast n
      Right _ -> when (e || i) $ putStrLn
        "options -e or -i are incompatible with -t"
