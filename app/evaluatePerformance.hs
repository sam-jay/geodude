import Evaluate
import System.Environment
import System.Exit(die)
import System.IO(readFile)
import Geometry(Point)
import Data.List.Split (splitOn)

main :: IO()
main = do 
 args <- getArgs
 case args of
  [loadMode, buildTreeMode, queryMode, numPoint, numPolygon] -> do
   let lm = getMode loadMode
       tm = getMode buildTreeMode
       qm = getMode queryMode
       numP = read numPoint :: Int
       numPolygon = read numPoint :: Int
   evaluate lm tm qm numP numPolygon
  _ -> do
   pn <- getProgName
   die $ "Usage: " ++ pn ++ " <loadFile mode> <makeTree mode> <queryPoint mode> <numPoint> <numPolygon>\nmode:s --sequential, p --parallel"

getMode :: String -> Execution
getMode "s" = Sequential
getMode "p" = Parallel
getMode _ = error "Wrong Input"