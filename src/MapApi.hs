
{-# LANGUAGE OverloadedStrings #-}
module MapApi where

import System.Environment -- to get environment variables
import Data.Text -- to pack strings to pass into lib functions

import Network.Wreq -- Main REST request lib
import Control.Lens -- Lib used to access results from requests
import Data.Aeson.Lens -- Json Access

--TODO: Add setup information about env-var to top level readme
--TODO: Maybe add funnctionallity to get web url for given request (https://msdn.microsoft.com/en-us/library/dn217138.aspx)

data Result = Location { name :: Text,
                        entityType :: Text,
                        confidence :: Text}

-- Print a location result
instance Show Result where
    show (Location name entityType confidence) = unpack name ++ " (" ++ unpack entityType ++ ", Confidence: " ++ unpack confidence ++ ")"


-- Main node of the rest request
mapsUrl = "http://dev.virtualearth.net/REST/v1/Locations"

sendQuery query =
    do
    apiKey <- getEnv "BING_API_KEY"
    let opts = defaults & param "query" .~ [pack query]
                & param "key" .~ [pack apiKey]
         in return(getWith opts mapsUrl)

readName response index = response^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "name" . _String

readEntityType response index =  response^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "entityType" . _String

readConfidence response index =  response^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth index . key "confidence" . _String


-- Maybe rewrite to return a list of all results
getLocation query  = do
    apiKey <- getEnv "BING_API_KEY"
    let opts = defaults & param "query" .~ [pack query]
                        & param "key" .~ [pack apiKey]
                        & param "c" .~ [pack "de"]
                        & param "userRegion" .~ [pack "DE"]
                        & param "userLocation" .~ [pack "48.153737,11.552366"]
    r <- getWith opts mapsUrl
    return( Location (readName r 0)
                     (readEntityType r 0)
                     (readConfidence r 0))
