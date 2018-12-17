
{-# LANGUAGE OverloadedStrings #-}
module MapApi where --(Location(..), getLocation) where

import System.Environment -- to get environment variables
import Data.Text -- to pack strings to pass into lib functions

import Network.Wreq -- Main REST request lib
import Control.Lens -- Lib used to access results from requests
import Data.Aeson.Lens -- Json Access


data Location = Location { name :: String,
                        entityType :: String,
                        confidence :: String}

-- Print a location result
instance Show Location where
    show (Location name entityType confidence) = "{" ++ name ++ " (" ++ entityType ++ ", Confidence: " ++ confidence ++ ")}"


-- Main node of the rest request
mapsUrl :: String
mapsUrl = "http://dev.virtualearth.net/REST/v1/Locations"


getLocation :: String -> IO [Location]
getLocation query  = if query == ""
                        then return []
                        else
                            do
                                apiKey <- getEnv "BING_API_KEY"
                                let opts = defaults & param "query" .~ [pack query]
                                                    & param "key" .~ [pack apiKey]
                                                    & param "c" .~ [pack "de"]
                                                    & param "userRegion" .~ [pack "DE"]
                                                    & param "userLocation" .~ [pack "48.153737,11.552366"]
                                response <- getWith opts mapsUrl
                                let numResult = fromIntegral (Prelude.head (numberOfResources response))
                                    range =  [0..numResult-1]
                                return (Prelude.map (readLocation response) range)

-- Access functions for Location-json fields
-- These are not typed because the types rely on the Json implementation which might vary

readName response index = response ^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "name" . _String

readEntityType response index =  response ^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "entityType" . _String

readConfidence response index =  response ^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "confidence" . _String

numberOfResources response = response ^.. responseBody . key "resourceSets" . nth 0 . key "estimatedTotal"  . _Integer

readLocation response index = Location
                                (unpack (readName response index))
                                (unpack (readEntityType response index))
                                (unpack (readConfidence response index))


