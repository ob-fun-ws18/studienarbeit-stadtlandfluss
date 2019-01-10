module IsTypeSpec(spec) where

import Data.Maybe
import GameLogic
import Test.Hspec
import Test.QuickCheck
import MapApi

spec :: Spec
spec = describe "isType" $ do
        it "returns true if type matches" $
            (isType "River" (Location "Isar" "River" "high"))
                `shouldBe` True
        it "returns false if type does not match" $
            (isType "River" (Location "Germany" "CityRegion" "low"))
                `shouldBe` False
