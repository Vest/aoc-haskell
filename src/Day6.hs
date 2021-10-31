module Day6 where

import Data.List (group, sort, sortOn, transpose)
import Data.Ord

parseInput :: String -> [String]
parseInput = transpose . lines

chooseMostFrequent :: String -> Char
chooseMostFrequent =
  snd
    . head
    . sortOn (Down . fst)
    . map (\l -> (length l, head l))
    . group
    . sort

chooseLeastFrequent :: String -> Char
chooseLeastFrequent =
  snd
    . head
    . sortOn fst
    . map (\l -> (length l, head l))
    . group
    . sort

solution1 :: String -> String
solution1 = map chooseMostFrequent . parseInput

solution2 :: String -> String
solution2 = map chooseLeastFrequent . parseInput

solution :: String -> String
solution input = solution1 input ++ ", " ++ solution2 input
