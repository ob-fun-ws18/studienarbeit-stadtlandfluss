module CalcBonusFactorSpec(spec) where

import Data.Maybe
import GameLogic
import Test.Hspec
import Test.QuickCheck
import MapApi

spec :: Spec
spec = describe "calcBonusFactor" $ do
        it "gives factor 4 if only one non empty answer was given" $
            (calcBonusFactor "Bla" ["", "Bla", "",""])
                `shouldBe` 4
        it "gives factor 2 if answer is unique but other answers where given" $
            (calcBonusFactor "Bla" ["Hallo", "Bla", "Fasel",""])
                `shouldBe` 2
        it "gives factor 1 if answer is not unique" $
            (calcBonusFactor "Bla" ["Bla", "", "Fasel","Bla"])
                `shouldBe` 1