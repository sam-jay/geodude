import Evaluate
import System.Environment
import System.Exit(die)
import System.IO(readFile)
import Geometry(Point)
import Data.List.Split (splitOn)

main :: IO ()
main = do
 args <- getArgs
 case args of
  [filename, "s"] -> do 
   contents <- readFile filename
   let points = getPoints $ lines contents
   evaluateList Sequential points
  [filename, "p"] -> do
   contents <- readFile filename
   let points = getPoints $ lines contents
   evaluateList Parallel points
  _ -> do 
   pn <- getProgName
   die $ "Usage: " ++ pn ++ " <fileName> <mode> \nmode:s --sequential, p --parallel" 

getPoints :: [String] -> [Point]
getPoints lines = map toPoint lines
 where toPoint l = helper $ splitOn "," l
       helper [xs, ys] = (read xs :: Double, read ys :: Double)
       helper [a] = error ("Error format: " ++ a)