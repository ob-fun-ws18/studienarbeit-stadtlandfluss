
{-# LANGUAGE OverloadedStrings #-}
module MapApi where

import System.Environment
import Data.Text
import Network.Wreq
import Control.Lens

--TODO: Add setup information about env-var to top level readme


mapsUrl = "http://dev.virtualearth.net/REST/v1/Locations"

tmp = do
    apiKey <- getEnv("BING_API_KEY")
    opts <- return(defaults & param "query" .~ ["donau"]
                    & param "key" .~ [pack(apiKey)])
    r <- getWith opts mapsUrl
    return(r ^. responseBody)
