import Rcl.Ast
import Rcl.Cli

rclFiles :: [String]
rclFiles = map ("examples/" ++) ["AhnDiss.rcl"
  , "SyntaxTest.rcl"
  , "TypeErrors.rcl"
  , "AhnSandhuPaper2000.rcl"]

binary :: String
binary = "rcl2000"

main :: IO ()
main = do
  mapM_ (\ f -> cli binary $ ["-f", show f] ++ rclFiles) forms
  mapM_ (\ f -> cli binary $ ["-n", "-f", show f] ++ rclFiles) forms
  mapM_ (\ o -> cli binary $ o : rclFiles) ["-h", "-c", "-r", "-e"]
  mapM_ (\ o -> cli binary $ o : "-t" : rclFiles) ["-c", "-r"]
  cli binary $ ["-o", "test"] ++ rclFiles
  cli binary $ ["-t", "-o", "testt"] ++ rclFiles
