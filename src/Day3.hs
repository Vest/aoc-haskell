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
solution input = (show . solution1 $ input) ++ ", " ++ (show . solution2 $ input)

transposeList :: [Triangle] -> [Triangle]
transposeList [t1, t2, t3] =
  let (t1x, t1y, t1z) = t1
      (t2x, t2y, t2z) = t2
      (t3x, t3y, t3z) = t3
   in [(t1x, t2x, t3x), (t1y, t2y, t3y), (t1z, t2z, t3z)]
transposeList (t1 : t2 : t3 : ts) = transposeList [t1, t2, t3] ++ transposeList ts
transposeList _ = []

solution2 :: String -> Int
solution2 = length . filter isValid . transposeList . parseInput
