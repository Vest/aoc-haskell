module Day10 where

import Data.List.Split (endBy)
import Data.Map as M

data Bot = Bot
  { low,
    high ::
      Int
  }

data TokenValue = IntValue Int | BotValue Int | OutputValue Int
  deriving (Eq, Show)

parseLine :: String -> Maybe [TokenValue]
parseLine input =
  let tokens = endBy " " input
      tokensCount = length tokens
   in case tokensCount of
        6 -> Just . parseSix $ tokens
        12 -> Just . parseTwelve $ tokens
        _ -> Nothing

parseSix :: [String] -> [TokenValue]
parseSix input =
  let valueString = input !! 1
      value = read valueString :: Int
      botString = last input
      bot = read botString :: Int
   in [IntValue value, BotValue bot]

parseTwelve :: [String] -> [TokenValue]
parseTwelve input =
  let botString = input !! 1
      bot = read botString :: Int
      botName = head input
      firstString = input !! 6
      first = read firstString :: Int
      firstName = input !! 5
      secondString = last input
      second = read secondString :: Int
      secondName = input !! 10
      createValue name value = if name == "bot" then BotValue value else OutputValue value
   in [ createValue botName bot,
        createValue firstName first,
        createValue secondName second
      ]

solution :: String -> String
solution _ = "output"
