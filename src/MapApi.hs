
{-# LANGUAGE OverloadedStrings #-}
module MapApi where

import Network.Wreq
import Control.Lens

tmp = do
    r <- get "http://httpbin.org/get"
    return(r ^. responseBody)