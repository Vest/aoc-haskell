module Day6 where

import Data.List (group, sort, sortOn, transpose)
import Data.Ord

data Frequency = MOST | LEAST

parseInput :: String -> [String]
parseInput = transpose . lines

chooseFrequent :: Frequency -> String -> Char
chooseFrequent freq =
  snd
    . head
    . sortOn
      ( \(len, _) -> case freq of
          MOST -> negate len
          LEAST -> len
      )
    . map (\l -> (length l, head l))
    . group
    . sort

solution1 :: String -> String
solution1 = map (chooseFrequent MOST) . parseInput

solution2 :: String -> String
solution2 = map (chooseFrequent LEAST) . parseInput

solution :: String -> String
solution input = solution1 input ++ ", " ++ solution2 input
