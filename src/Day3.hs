module Day3 where

import Data.List.Split
import Data.Maybe

type Triangle = (Int, Int, Int)

buildTriangle :: [Int] -> Maybe Triangle
buildTriangle (x : y : z : _) = Just (x, y, z)
buildTriangle _ = Nothing

isValid :: Triangle -> Bool
isValid (x, y, z) = x + y > z && x + z > y && y + z > x

parseLine :: String -> Maybe Triangle
parseLine = buildTriangle . map read . split (dropDelims . dropBlanks $ onSublist " ")

parseInput :: String -> [Triangle]
parseInput = mapMaybe parseLine . lines

solution1 :: String -> Int
solution1 = length . filter isValid . parseInput

solution :: String -> String
solution = show . solution1
