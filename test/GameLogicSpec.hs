module GameLogicSpec(spec) where

import GameLogic
import Test.Hspec
import Test.QuickCheck
import MapApi

-- Note: not all functions of the Gamelogic are tested, because some rely on IO-Calls to the maps api.

spec :: Spec
spec =
 describe "scoreAnswer" $ do
  it "gives 0 points if no location is found" $
   fst (scoreAnswer "River" [] "" ["", "isar", "donau"]) `shouldBe` 0
  it "Gives confidence 'Invalid' if no location is found" $
   snd (scoreAnswer "River" [] "" ["", "isar", "donau"]) `shouldBe` "Invalid"
  it "gives 0 points for a answer of wrong type" $
   fst (scoreAnswer
            "CountryRegion"
            [Location "Isar" "River" "high", Location "Isar" "River" "medium"]
            "Isar"
            ["Isar", "Indien", "", "Italien"]) `shouldBe` 0
  it "gives 5 points for a correct non unique answer" $
   fst (scoreAnswer
            "River"
            [Location "Isar" "River" "high", Location "Isar" "River" "medium"]
            "Isar"
            ["Isar", "Isar", "", "Italien"]) `shouldBe` 5
  it "gives 10 points for a correct unique answer" $
   fst (scoreAnswer
            "River"
            [Location "Isar" "River" "high", Location "Isar" "River" "medium"]
            "Isar"
            ["", "Isar", "", "Italien"]) `shouldBe` 10
  it "gives 20 points if its the only answer" $
   fst (scoreAnswer
            "River"
            [Location "Isar" "River" "high", Location "Isar" "River" "medium"]
            "Isar"
            ["", "", "Isar", ""]) `shouldBe` 20