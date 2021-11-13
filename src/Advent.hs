{-# LANGUAGE OverloadedStrings #-}

module Advent (getAnswers) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Functor
import Day1 (solution)
import Day2 (solution)
import Day3 (solution)
import Day4 (solution)
import Day5 (solution)
import Day6 (solution)
import Day7 (solution)
import Day8 (solution)
import Day9 (solution)
import Day10 (solution)
import Network.HTTP.Simple

getAnswers :: String -> IO [String]
getAnswers token =
  sequence
    [ input 1 <&> Day1.solution,
      input 2 <&> Day2.solution,
      input 3 <&> Day3.solution,
      input 4 <&> Day4.solution,
      input 5 <&> Day5.solution,
      input 6 <&> Day6.solution,
      input 7 <&> Day7.solution,
      input 8 <&> Day8.solution,
      input 9 <&> Day9.solution,
      input 10 <&> Day10.solution
    ]
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
