module GameLogic where --(checkCity, checkCountry, checkRiver, currentScores) where

import MapApi
import Data.Maybe

-- Valid types are: CountryRegion (Land), River (Fluss), PopulatedPlace (Stadt)

checkCity :: [string] -> [(Int, String)]
checkCity cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

checkCountry :: [string] -> [(Int, String)]
checkCountry cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

checkRiver :: [string] -> [(Int, String)]
checkRiver cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]



bestMatch query expectedType = do
        locations <- getLocation query
        return (listToMaybe (filter (isType expectedType) locations))

isType :: String -> Location -> Bool
isType expetected location = (entityType location) == expetected

hasConfidence :: String -> Location -> Bool
hasConfidence expected location = (confidence location) == expected



currentScores :: [Int]
currentScores = [300, 0, 42, 30]
