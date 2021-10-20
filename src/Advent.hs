{-# LANGUAGE OverloadedStrings #-}

module Advent (getAnswers) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Functor
import Day1 (solution1)
import Network.HTTP.Simple

getAnswers :: String -> IO [String]
getAnswers token = sequence [input 1 <&> (show . Day1.solution1)]
  where
    input = getInput token

getInput :: String -> Integer -> IO String
getInput token day = do
  initialRequest <- parseRequest ("https://adventofcode.com/2016/day/" ++ show day ++ "/input")
  let request =
        setRequestMethod "GET" $
          setRequestHeader "Cookie" [getCookieValue ["session=", token]] initialRequest
  response <- httpBS request
  return . BSU.toString $ getResponseBody response

getCookieValue :: [String] -> BS.ByteString
getCookieValue = BS.concat . map BSU.fromString
