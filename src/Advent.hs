module Advent where

import Day1 (solution1)
import Network.HTTP.Client

getAnswers :: String -> String
getAnswers token = "Token is " ++ token
