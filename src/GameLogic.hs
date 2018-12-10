module GameLogic where --(checkCity, checkCountry, checkRiver, currentScores) where

import MapApi
import Data.Maybe

data Answer = Answer { location :: IO (Maybe Location), query :: String}
-- instance Show Answer where
--    show (Answer loc query) = "{" ++ (maybe "<No Location found>" (name) loc) ++ "}"


-- Valid types are: CountryRegion (Land), River (Fluss), PopulatedPlace (Stadt)

checkCity :: [string] -> [(Int, String)]
checkCity cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

checkCountry :: [string] -> [(Int, String)]
checkCountry cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

checkRiver :: [string] -> [(Int, String)]
checkRiver cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

checkRiverIo :: [String] -> IO [ (Int, String)]
checkRiverIo answers =  mapM (evaluateAnswer "River" answers) answers

checkCountryIo :: [String] -> IO [ (Int, String)]
checkCountryIo answers =  mapM (evaluateAnswer "CountryRegion" answers) answers

checkCityIo :: [String] -> IO [ (Int, String)]
checkCityIo answers =  mapM (evaluateAnswer "PopulatedPlace" answers) answers

evaluateAnswer :: String -> [String] -> String -> IO (Int, String)
evaluateAnswer queryType allQueries query =
        do locations <- getLocation query
           return (scoreAnswer queryType locations query allQueries)



scoreAnswer :: String -> [Location] -> String -> [String] -> (Int, String)
scoreAnswer queryType locs query allQueries
    = let loc = bestMatch queryType locs
          conf = maybe "Invalid" confidence loc
          points = maybe 0 (const 5) loc
          bonusFactor = calcBonusFactor query allQueries
      in (points * bonusFactor, conf)

bestMatch :: String -> [Location] -> Maybe Location
bestMatch expectedType locations = listToMaybe (filter (isType expectedType) locations)

isType :: String -> Location -> Bool
isType expetected location = (entityType location) == expetected

calcBonusFactor :: String -> [String] -> Int
calcBonusFactor query allQueries = if (length (filter (== query) allQueries)) > 1
                                    then 1 -- Answer is not unique
                                    else if (length (filter (== "") allQueries)) == (length allQueries) - 1
                                            then 4 -- query was the only answer
                                            else 2 -- query was unique

-- currently unused, if too many false answer are accepted filtering out "low" results might help
hasConfidence :: String -> Location -> Bool
hasConfidence expected location = (confidence location) == expected
{-|
tmp inputs = do
        answers <- map (Answer 10) inputs
        answers <- map (checkForEmpty) [answers]
        answers <- map (checkUniques [answers]) [answers]
        return answers
      --  answers <- map ()
checkUniques :: [Answer] -> Answer -> Answer
checkUniques allAnswers (Answer oldScore inQuery)
    = let allQueries = map query allAnswers
          newScore = if (length (filter (== inQuery) allQueries)) > 1
                        then oldScore
                        else oldScore * 2
        in Answer newScore inQuery

tmpBla allAnswers (Answer oldScore inQuery)
    = let allQueries = map query allAnswers
         in length (filter (== inQuery) allQueries)


checkForEmpty :: Answer -> Answer
checkForEmpty (Answer points query) = if query == ""
                                        then (Answer 0 query)
                                        else (Answer points query)


contains list member = (filter (== member) list) /= []
-}




currentScores :: [Int]
currentScores = [300, 0, 42, 30]
