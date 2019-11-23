import GeoJSONParser( parseCountry, parseState )
import qualified Data.ByteString.Lazy as B
import Control.Monad
main :: IO ()
main = do
	content <- B.readFile "../data/states_provinces.json"
	let res = parseState content
	let len = res >>= (\xs -> Just (length xs))
	putStrLn ("total length:" ++ show len)