import Control.Monad (filterM)

import Rcl.Ast
import Rcl.Cli

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (takeExtension, (</>))

main :: IO ()
main = do
  let e = "examples"
      d = "test" </> "expectedParseErrors"
      b = "rcl2000"
  es <- getDirectoryContents e
  let rs = map (e </>) $ filter ((== ".rcl") . takeExtension) es
  mapM_ (\ f -> cli b $ ["-f", show f] ++ rs) forms
  mapM_ (\ f -> cli b $ ["-n", "-f", show f] ++ rs) forms
  mapM_ (\ o -> cli b $ o : rs) ["-h", "-c", "-r", "-e"]
  mapM_ (\ o -> cli b $ o : "-t" : rs) ["-c", "-r"]
  cli b $ "-otest" : rs
  cli b $ ["-t", "-o"] ++ rs
  ts <- getDirectoryContents d
  fs <- filterM doesFileExist $ map (d </>) ts
  cli b fs
