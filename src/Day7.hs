module Day7 where

import Data.List (find)
import Data.List.Split (splitOneOf)
import Data.Maybe (isJust, isNothing)

data IPv7 = IPv7
  { address,
    hypenet ::
      [String]
  }
  deriving (Show, Eq)

parseLine :: String -> IPv7
parseLine =
  foldl
    ( \IPv7 {address = addr, hypenet = hyp} (num, chars) ->
        if even num
          then IPv7 {address = addr ++ [chars], hypenet = hyp}
          else IPv7 {address = addr, hypenet = hyp ++ [chars]}
    )
    IPv7 {address = [], hypenet = []}
    . zip [0 :: Integer ..]
    . splitOneOf "[]"

isABBA :: String -> Bool
isABBA (a : b : c : d : abba) =
  (a == d && b == c && a /= b) || isABBA (b : c : d : abba)
isABBA _ = False

isIPv7Valid :: IPv7 -> Bool
isIPv7Valid IPv7 {address = addr, hypenet = hyp} = (isJust . find isABBA $ addr) && (isNothing . find isABBA $ hyp)

solution :: String -> String
solution = show . solution1

solution1 :: String -> Int
solution1 = length . filter isIPv7Valid . map parseLine . lines
