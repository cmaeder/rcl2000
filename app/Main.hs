module Main where

import Rcl.Cli (cli)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  prN <- getProgName
  args <- getArgs
  cli prN args
