{-|
Module      : GameLogic
Description : Contains functions to score possible Answers

This module contains the "Brain" of the Game. It allows input to be checked using the MapApi Lib
and it allows the scoring of answers.
-}

module GameLogic where --(checkCity, checkCountry, checkRiver, currentScores) where

import Data.IORef

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import MapApi
import Data.Maybe
import Test.RandomStrings

-- | Generates a random uppercase Char
getRandomChar :: IO Char -- ^ Random uppercase char
getRandomChar = onlyAlpha' 1 randomASCII

-- Valid types are: CountryRegion (Land), River (Fluss), PopulatedPlace (Stadt). For others see bing api doc

-- | Checks and scores a list of answers for the river type
checkRiver :: [String] -- ^ List of all answers given
             -> IO [ (Int, String)] -- ^ Score and likelihood of every answer
checkRiver answers =  mapM (evaluateAnswer "River" answers) answers

-- | Checks and scores a list of answers for the country type
checkCountry :: [String] -- ^ List of all answers given
               -> IO [ (Int, String)] -- ^ Score and likelihood of every Answer
checkCountry answers =  mapM (evaluateAnswer "CountryRegion" answers) answers

-- | Checks and scores a list of answers for the city type
checkCity :: [String] -- ^ List of all answers given
            -> IO [(Int, String)] -- ^ Score and likelihood of every Answer
checkCity answers =  mapM (evaluateAnswer "PopulatedPlace" answers) answers

-- | Evaluates how likely a given answer is to be correct and how many points it should be worth
-- Uses the Rest API to get all Locations matching the Answer.
evaluateAnswer :: String -- ^ The entity type of the answer CountryRegion | River | PopulatedPlace
               -> [String] -- ^ List of all Answers to determine score multiplier
               -> String -- ^ The given Answer which will be scored
               -> IO (Int, String) -- ^ Score and likelihood
evaluateAnswer queryType allQueries query =
        do locations <- getLocation query
           return (scoreAnswer queryType locations query allQueries)

-- | Score a answer based on given locations
scoreAnswer :: String -- ^ Entity Type which the location should have
            -> [Location] -- ^ List of locations matching the query
            -> String -- ^ Query / Answer to be scored
            -> [String] -- ^ All Answers used for score multiplier
            -> (Int, String) -- ^ Score and likelihood
scoreAnswer queryType locs query allQueries
    = let loc = bestMatch queryType locs
          conf = maybe "Invalid" confidence loc
          points = maybe 0 (const 5) loc
          bonusFactor = calcBonusFactor query allQueries
      in (points * bonusFactor, conf)

-- | Filter a sorted List of locations for the best match in a given entity Type
bestMatch :: String -- ^ Entity type which the result MUST match
          -> [Location] -- ^ List of all Locations
          -> Maybe Location -- ^ Best Match for given type, nothing if no result for given type was found
bestMatch expectedType locations = listToMaybe (filter (isType expectedType) locations)

-- | determine if Location type matches
isType :: String -- ^ Name of the type expected
       -> Location -- ^ Location to be checked
       -> Bool -- ^ true if entityType is as expected
isType expetected location = entityType location == expetected

-- | Calculates a score multiplier for a given Answer
-- 4: if only one non empty answer was given. (No one else answered in that category)
-- 2: The answer is unique. (There where other answers, but not equal answers)
-- 1: The answer was given multiple times.
-- Note: Empty answers will get score multipliers too, but since they will receive 0 points the multiplier does not matter
calcBonusFactor :: String -- ^ Answer for which the multiplier is calculated
                -> [String] -- ^ List of all Answers (including the one being evaluated)
                -> Int -- ^ Score multiplier (1 | 2 | 4)
calcBonusFactor query allQueries | length (filter (== query) allQueries) > 1 = 1 -- Duplicate answer
                                 | length (filter (== "") allQueries) == length allQueries - 1 = 4 -- Only answer
                                 | otherwise = 2 -- Unique answer


currentScores :: [Int]
currentScores = [0, 0, 0, 0]
-- TODO: Find out a good way to store scores

