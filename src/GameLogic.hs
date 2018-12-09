module GameLogic (checkCity, checkRiver, checkCountry, currentScores, score) where

import Data.IORef
score :: IORef Int
score = return (newIORef 0)

checkCity :: [string] -> [(Int, String)]
checkCity cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

checkRiver :: [string] -> [(Int, String)]
checkRiver cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

checkCountry :: [string] -> [(Int, String)]
checkCountry cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

currentScores :: [Int]
currentScores = [300, 0, 42, 30]