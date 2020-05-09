module Rcl.Cli (cli) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find)
import qualified Data.Map as Map (empty)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

import Paths_rcl2000 (getDataFileName, version)
import Rcl.Ast (Stmt, UserTypes)
import Rcl.Data (Model, getUserTypes)
import Rcl.Eval (evalInput)
import Rcl.Interpret (interprets)
import Rcl.Model (initModel)
import Rcl.Parse (ParseError, parser)
import Rcl.Print (Form (Form), Format (..), pStmts, render)
import Rcl.Read (readModel, readMyFile, readTypes)
import Rcl.Reduce (reduction)
import Rcl.ToOcl (ocl)
import Rcl.ToSoil (toSoil)
import Rcl.Type (typeErrors)

import System.Console.GetOpt
import System.FilePath (hasExtension, replaceDirectory, replaceExtension,
                        takeFileName, (</>))
import Text.ParserCombinators.Parsec (parse)

cli :: String -> [String] -> IO ()
cli prN args = case getOpt Permute options args of
      (os, n, []) -> let o = foldl (flip id) dOpts os in
        if help o then putStrLn $
          usageInfo ("usage: " ++ prN ++ " [options] <file>*") options
        else if vers o then putStrLn $ prN ++ " Version " ++ showVersion version
        else let
          rm = readModel $ map (optsFile o)
            [rhFile, uaFile, paFile, sessFile, setsFile]
          in case n of
        [] -> if stmtOpts o then
          putStrLn "unexpected options without file arguments"
          else rm >>= evalInput [] . initModel
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
    , Option "v" ["version"]
      (NoArg $ \ o -> o {vers = True})
      "show version"
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
    , Option "o" ["out-dir"]
      (OptArg (\ mt o -> let o1 = o {toOcl = True} in
                  maybe o1 (\ t -> o1 {outDir = t}) mt) "dir")
      $ "write .use (and .soil) file(s) to directory, default: "
      ++ outDir dOpts
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
  , vers :: Bool
  , dir :: FilePath
  , ext :: String
  , typesFile :: FilePath
  , rhFile :: FilePath
  , uaFile :: FilePath
  , paFile :: FilePath
  , sessFile :: FilePath
  , setsFile :: FilePath
  , useFile :: FilePath
  , outDir :: FilePath }

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
  , vers = False
  , dir = "examples"
  , ext = "txt"
  , typesFile = "types"
  , rhFile = "rh"
  , uaFile = "ua"
  , paFile = "pa"
  , sessFile = "s"
  , setsFile = "sets"
  , useFile = "use/RBAC.use"
  , outDir = "use" }

optsFile :: Opts -> (Opts -> FilePath) -> FilePath
optsFile o sel = let f = sel o in
  (if takeFileName f == f then (dir o </>) else id)
  $ if hasExtension f then f else replaceExtension f $ ext o

stmtOpts :: Opts -> Bool
stmtOpts o = any (\ f -> f o) [pprint, check, reduce, evaluate, toOcl]

onlyPrint :: Opts -> Bool
onlyPrint o = not (prompt o) && (pprint o || not (stmtOpts o))

form :: Opts -> Form
form o = let low = map toLower in
  Form (fromMaybe Uni $ find (\ f -> low (show f) == low (format o))
    [LaTeX, Ascii]) $ parens o

processFile :: Either Model UserTypes -> Opts -> FilePath -> IO ()
processFile eith o file = do
  str <- readMyFile file
  reportParse eith o file $ parse parser file str

reportParse :: Either Model UserTypes -> Opts -> FilePath
  -> Either ParseError [Stmt] -> IO ()
reportParse mus o file eith = case eith of
  Left err -> print err
  Right ast -> do
    let p = pprint o
        c = check o
        r = reduce o
        t = toOcl o
        e = evaluate o
        i = prompt o
        use = replaceDirectory (replaceExtension file "use") $ outDir o
        us = either getUserTypes id mus
    when (p || onlyPrint o) . putStrLn . render $ pStmts (form o) ast
    when c . putStrLn $ typeErrors us ast
    when r . putStrLn $ reduction us ast
    when t $ do
      let uf = useFile o
      str0 <- readMyFile uf
      str <- if null str0 then do
          f <- getDataFileName uf
          readMyFile f
        else return str0
      writeFile use $ str ++ ocl us ast
      case mus of
        Left m -> writeFile (replaceExtension use "soil") $ toSoil m
        _ -> putStrLn "no .soil file written for option -t"
    case mus of
      Left m -> do
        let n = initModel m
        when e . putStrLn $ interprets us n ast
        when i $ evalInput ast n
      Right _ -> when (e || i) $ putStrLn
        "options -e or -i are incompatible with -t"
