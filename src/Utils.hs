{-# LANGUAGE OverloadedStrings #-}
module Utils (fetchURL) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

fetchURL :: String -> IO String
fetchURL url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ L8.unpack $ getResponseBody response

