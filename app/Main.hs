module Main where

import Rcl.Ast
import Rcl.Parse
import Rcl.Print
import Rcl.Type
import System.Environment
import System.Exit
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      str <- getContents
      if null str then die "expected file arguments" else
        reportParse $ parse parser "" str
    _ -> mapM_ parseFile args

parseFile :: String -> IO ()
parseFile file = parseFromFile parser file >>= reportParse

reportParse :: Either ParseError [Stmt] -> IO ()
reportParse eith = case eith of
    Left err -> do
        print err
        exitFailure
    Right ast -> do
        let line = putStrLn "////////////////////"
        line
        putStrLn (pp True ast)
        line
        putStrLn (pLaTeX True ast)
        line
        putStrLn (pAscii True ast)
        line
        putStrLn (pp False ast)
        line
        putStrLn (pLaTeX False ast)
        line
        putStrLn (pAscii False ast)
        putStrLn $ exec ast
        exitSuccess
