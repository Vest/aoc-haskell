{-# LANGUAGE LambdaCase #-}

module Day2 where

import qualified Data.List as L

data Direction = U | D | L | R | Empty
  deriving (Eq, Show)

{-1 2 3
  4 5 6
  7 8 9-}
step :: Int -> Direction -> Int
step num U | num > 3 = num - 3
step num D | num < 7 = num + 3
step num L | num `notElem` [1, 4, 7] = num - 1
step num R | num `notElem` [3, 6, 9] = num + 1
step num _ = num

parseLine :: String -> [Direction]
parseLine =
  filter (/= Empty)
    . map
      ( \case
          'U' -> U
          'D' -> D
          'L' -> L
          'R' -> R
          _ -> Empty
      )

detectNumber :: Int -> [Direction] -> Int
detectNumber 0 = foldl step 5
detectNumber n = foldl step n

solution1 :: String -> Int
solution1 =
  fst
    . foldl
      ( \(answer, startNum) dirs ->
          let newNum = detectNumber startNum dirs
           in (newNum + 10 * if (startNum `mod` 10) /= 0 then answer else startNum, newNum)
      )
      (5, 0)
    . map parseLine
    . lines

solution :: String -> String
solution input = (show . solution1 $ input)
