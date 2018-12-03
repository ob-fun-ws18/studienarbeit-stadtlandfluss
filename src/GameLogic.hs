module GameLogic (checkCity, currentScores) where

checkCity :: [string] -> [(Int, String)]
checkCity cities = [(42, "high"), (0, "invalid"), (20, "high"), (10, "low")]

currentScores :: [Int]
currentScores = [300, 0, 42, 30]