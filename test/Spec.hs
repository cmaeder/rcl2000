import Control.Monad (filterM)

import Rcl.Ast
import Rcl.Cli

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (takeExtension, (</>))

main :: IO ()
main = do
  let e = "examples"
      d = "test" </> "expectedParseErrors"
      b = cli "rcl2000"
  es <- getDirectoryContents e
  let rs = map (e </>) $ filter ((== ".rcl") . takeExtension) es
  mapM_ (\ f -> b $ ["-f", show f] ++ rs) forms
  mapM_ (\ f -> b $ ["-n", "-f", show f] ++ rs) forms
  mapM_ (\ o -> b $ o : "-t" : rs) ["-c", "-r"]
  b $ ["-t", "-o"] ++ rs
  mapM_ (\ o -> b $ o : rs) ["-h", "-c", "-r", "-e"]
  mapM_ (\ v -> b $ ("-v" ++ show v) : "-otest" : rs) [0 .. 2 :: Int]
  ts <- getDirectoryContents d
  fs <- filterM doesFileExist $ map (d </>) ts
  b fs
