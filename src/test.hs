import GeoJSONParser (parseFeatureCollection)

import qualified Data.ByteString.Lazy as B

loadStates = do
    x <- B.readFile "../data/states_provinces.json"
    return $ parseFeatureCollection x

loadCountries = do
    x <- B.readFile "../data/countries.json"
    return $ parseFeatureCollection x

