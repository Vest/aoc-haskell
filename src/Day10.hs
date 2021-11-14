module Day10 where

import Data.List.Split (endBy)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Bot = Bot
  { low,
    high ::
      ValueLocation
  }
  deriving (Eq, Show)

data ValueLocation = BotLocation Int | OutputLocation Int
  deriving (Eq, Show)

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

parseInput :: String -> (M.Map Int Bot, M.Map Int ValueLocation)
parseInput =
  foldl
    ( \(rules, values) tokens ->
        let tokensCount = length tokens
         in if tokensCount == 2
              then (rules, updateValuesMap values tokens)
              else (updateBotsMap rules tokens, values)
    )
    (M.empty, M.empty)
    . mapMaybe parseLine
    . lines

updateValuesMap :: M.Map Int ValueLocation -> [TokenValue] -> M.Map Int ValueLocation
updateValuesMap m [IntValue i, BotValue b] = M.insert i (BotLocation b) m
updateValuesMap m _ = m

updateBotsMap :: M.Map Int Bot -> [TokenValue] -> M.Map Int Bot
updateBotsMap m [BotValue b, l, h] =
  let lowMaybe = extractLocation l
      highMaybe = extractLocation h
      extractLocation v = case v of
        BotValue bv -> Just . BotLocation $ bv
        OutputValue ov -> Just . OutputLocation $ ov
        _ -> Nothing
   in case (lowMaybe, highMaybe) of
        (Just lv, Just hv) -> M.insert b Bot {low = lv, high = hv} m
        _ -> m
updateBotsMap m _ = m

solution :: String -> String
solution _ = "output"
