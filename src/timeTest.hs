import GeoJSONParser (parseFeatureCollection, GeoJSONFeatureCollection)
import qualified Data.ByteString.Lazy as B
import qualified Entities as E
-- import Data.Time


main ::IO()
main = do 
  content <- B.readFile "../data/states_provinces.json"
  case parseFeatureCollection content of
        Nothing -> error "error parsing feature collection"
        Just fc -> case E.parseStates fc of
                       Nothing -> error "error parsing states"
                       Just states -> do 
                          let state_len = length states
                          print state_len
                          -- let tree = fromList states
                          -- let dep = depth tree
                          -- putStr "build tree: "
                          -- putStrLn ("depth " ++ show dep)
                          
  --print $ B.length content
