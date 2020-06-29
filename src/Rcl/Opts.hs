module Rcl.Opts (Opts (..), getOpts, optsFile, usage) where

import Data.Char (isDigit)
import System.Console.GetOpt
import System.FilePath (hasExtension, replaceExtension, takeFileName, (</>))
usage :: String -> String
usage prN = usageInfo ("usage: " ++ prN ++ " [options] <file>*") options

getOpts :: [String] -> Either [String] (Opts, [FilePath])
getOpts args = case getOpt Permute options args of
  (os, n, []) -> Right (foldl (flip id) dOpts os, n)
  (_, _, es) -> Left es

optsFile :: Opts -> (Opts -> FilePath) -> FilePath
optsFile o sel = let f = sel o in
  (if takeFileName f == f then (dir o </>) else id)
  $ if hasExtension f then f else replaceExtension f $ ext o

-- | describe all available options
options :: [OptDescr (Opts -> Opts)]
options =
    [ Option "h" ["help"]
      (NoArg $ \ o -> o {help = True})
      "show help message"
    , Option "V" ["version"]
      (NoArg $ \ o -> o {vers = True})
      "show version"
    , Option "v" ["verbose"]
      (OptArg (\ mt o -> let o1 = o {verbose = 2} in
        maybe o1 (\ t -> o1 {verbose = case t of
                              _ | all isDigit t -> read t -- ignore others
                              _ -> verbose dOpts }) mt) "0|1|2")
      $ "verbose output, -v0 quiet, default: " ++ show (verbose dOpts)
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
      (OptArg (\ mt o -> let o1 = o {onlyType = True} in
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
  , verbose :: Int
  , format :: String
  , pprint :: Bool
  , check :: Bool
  , reduce :: Bool
  , evaluate :: Bool
  , onlyType :: Bool
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
  , puFile :: FilePath
  , sessFile :: FilePath
  , setsFile :: FilePath
  , useFile :: FilePath
  , outDir :: FilePath }

dOpts :: Opts
dOpts = Opts
  { parens = True
  , verbose = 1
  , format = "Uni"
  , pprint = False
  , check = False
  , reduce = False
  , evaluate = False
  , onlyType = False
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
  , puFile = "pu"
  , sessFile = "s"
  , setsFile = "sets"
  , useFile = "use/RBAC.use"
  , outDir = "use" }
