
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MapApi
Description : Contains functions to access the Bing Map Api

This module handles accessing the Bing Maps api to return Locations for a given Query.
-}
module MapApi (Location(..), getLocation) where

import System.Environment -- to get environment variables
import Data.Text -- to pack strings to pass into lib functions

import Network.Wreq -- Main REST request lib
import Control.Lens -- Lib used to access results from requests
import Data.Aeson.Lens -- Json Access

-- | Location describing a rest request result
data Location = Location { name :: String, -- ^ Name of the location found
                        entityType :: String, -- ^ Entitiy Type of the location (e.g. River)
                        confidence :: String -- ^ how confident the Api was that this is the correct result (low, medium or high)
                        }

-- Print a location result
instance Show Location where
    show (Location name entityType confidence) = "{" ++ name ++ " (" ++ entityType ++ ", Confidence: " ++ confidence ++ ")}"


-- | Main node of the rest request
mapsUrl :: String
mapsUrl = "http://dev.virtualearth.net/REST/v1/Locations"

-- | Get a list of all matches for the given query string
getLocation :: String -- ^ Query to be searched
            -> IO [Location] -- ^ List of all locations returned from the api. Can be empty
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

-- | Read in all relevant info from the given json to a Location Data object
readLocation response index = Location
                                (unpack (readName response index))
                                (unpack (readEntityType response index))
                                (unpack (readConfidence response index))

-- Access functions for Location-json fields
-- These are not typed because the types rely on the Json implementation which might vary

-- | Read the name of the nth Location from the response body
readName response index = response ^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "name" . _String

-- | Read the entity ype of the nth Location from the response body
readEntityType response index =  response ^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "entityType" . _String

-- | Read the confidence of the nth Location from the response body
readConfidence response index =  response ^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "confidence" . _String

-- | Read the info field in which the number of given responses is stored
numberOfResources response = response ^.. responseBody . key "resourceSets" . nth 0 . key "estimatedTotal"  . _Integer


