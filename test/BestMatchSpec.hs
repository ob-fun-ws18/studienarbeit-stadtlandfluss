{-# LANGUAGE OverloadedStrings #-}

module BestMatchSpec(spec) where

import Data.Maybe
import GameLogic
import Test.Hspec
import Test.QuickCheck
import MapApi

spec :: Spec
spec = describe "bestMatch" $ do
        it "filters out locations with different type" $
            isNothing (bestMatch "River"
            [Location "Italien" "CountryRegion" "high", Location "Germany" "CountryRegion" "high"])
                `shouldBe` True
        it "shows the first result if multiple have the same type" $
            maybe False (\x -> name x == "Danube")
                (bestMatch "River"
                [Location "Danube" "River" "high", Location "Donau" "River" "low"])
                `shouldBe` True
        it "shows the first result of correct type" $
                maybe False (\x -> name x == "Donau")
                    (bestMatch "River"
                    [Location "Dresden" "CountryRegion" "high", Location "Donau" "River" "low"])
                    `shouldBe` True
