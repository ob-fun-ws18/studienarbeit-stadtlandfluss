
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


-- Maybe rewrite to return a list of all results
getLocation query  = do
    apiKey <- getEnv "BING_API_KEY"
    let opts = defaults & param "query" .~ [pack query]
                        & param "key" .~ [pack apiKey]
    r <- getWith opts mapsUrl
    -- Todo find a way to reuse partial results (like the resource node)
    return( Location (r^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth 0 . key "name" . _String)
                     (r^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth 0 . key "entityType" . _String)
                     (r^. responseBody . key "resourceSets" . nth 0 . key "resources" . nth 0 . key "confidence" . _String))
