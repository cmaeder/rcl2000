module Rcl.Cli (cli) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find)
import qualified Data.Map as Map (empty)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

import Paths_rcl2000 (getDataFileName, version)
import Rcl.Ast (Format (..), Let, UserTypes)
import Rcl.Data (Model)
import Rcl.Eval (evalInput, getAllUserTypes)
import Rcl.Interpret (interprets)
import Rcl.Model (initModel)
import Rcl.Opts
import Rcl.Parse (parser)
import Rcl.Print (Form (Form), pStmts, render)
import Rcl.Read (readModel, readMyFile, readTypes)
import Rcl.Reduce (reduction)
import Rcl.ToOcl (ocl)
import Rcl.ToSoil (toSoil)
import Rcl.Type (typeErrors)

import System.FilePath (replaceDirectory, replaceExtension)
import System.IO.CodePage (withCP65001)
import Text.ParserCombinators.Parsec (ParseError, parse)

-- | top level call with program name and arguments
cli :: String -> [String] -> IO ()
cli prN args = withCP65001 $ case getOpts args of
  Right (o, n) | help o -> putStrLn $ usage prN
    | vers o -> putStrLn $ prN ++ " Version " ++ showVersion version
    | stmtOpts o && null n -> putStrLn "missing file argument"
    | otherwise -> let
        v = verbose o
        rm = readModel v $ map (optsFile o)
          [rhFile, uaFile, paFile, puFile, sessFile, setsFile]
        in case n of
        [] -> rm >>= evalInput o [] . initModel
        _ -> do
          eith <- if onlyPrint o then return $ Right Map.empty else
            if onlyType o then fmap Right . readTypes v $ optsFile o typesFile
            else fmap Left rm
          mapM_ (processFile eith o) n
  Left errs -> mapM_ putStrLn errs

processFile :: Either Model UserTypes -> Opts -> FilePath -> IO ()
processFile eith o file = do
  str <- readMyFile (verbose o) file
  reportParse eith o file $ parse parser file str

reportParse :: Either Model UserTypes -> Opts -> FilePath
  -> Either ParseError [Let] -> IO ()
reportParse mus o file eith = case eith of
  Left err -> print err
  Right lets -> do
    let p = pprint o
        c = check o
        r = reduce o
        t = toOcl o
        e = evaluate o
        i = prompt o
        v = verbose o
        use = replaceDirectory (replaceExtension file "use") $ outDir o
        us = either getAllUserTypes id mus
    when (p || onlyPrint o) . putStrLn . render $ pStmts (form o) lets
    when c . putStrLn $ typeErrors us lets
    when r . putStrLn $ reduction us lets
    when t $ do
      let uf = useFile o
          (cs, res) = ocl us lets
      str0 <- readMyFile v uf
      str <- if null str0 then do
          f <- getDataFileName uf
          readMyFile v f
        else return str0
      writeMyFile v use $ str ++ res
      case mus of
        Left m -> writeMyFile v (replaceExtension use "soil") $ toSoil m cs
        _ -> when (v > 0) $ putStrLn
          "no .soil file written for option -t"
    case mus of
      Left m -> do
        let n = initModel m
        when e . putStrLn $ interprets us n lets
        when i $ evalInput o lets n
      Right _ -> when (e || i) $ putStrLn
        "options -e or -i are incompatible with -t"

writeMyFile :: Int -> FilePath -> String -> IO ()
writeMyFile v f s = do
  when (v > 0) . putStrLn $ "writing: " ++ f
  writeFile f s -- use files are ASCII
  when (v > 1) . putStrLn $ "successfully written: " ++ f

stmtOpts :: Opts -> Bool
stmtOpts o = any (\ f -> f o) [pprint, check, reduce, evaluate, toOcl]

onlyPrint :: Opts -> Bool
onlyPrint o = not (prompt o) && (pprint o || not (stmtOpts o))

form :: Opts -> Form
form o = let low = map toLower in
  Form (fromMaybe Uni $ find (\ f -> low (show f) == low (format o))
    [LaTeX, Ascii]) $ parens o
