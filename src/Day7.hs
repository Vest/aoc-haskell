module Day7 where

import Data.List (find, intersect)
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
solution input = (show . solution1 $input) ++ ", " ++ (show . solution2 $ input)

solution1 :: String -> Int
solution1 = length . filter isIPv7Valid . map parseLine . lines

isABA :: Char -> Char -> Char -> Bool
isABA a b c =
  a == c && a /= b

findABAs :: String -> [String]
findABAs str = map (\(a, b, c) -> [a, b, c]) . filter (\(a, b, c) -> isABA a b c) $ zip3 str (drop 1 str) (drop 2 str)

fromABAtoBAB :: String -> String
fromABAtoBAB [a, b, _] = [b, a, b]
fromABAtoBAB l = error $ "Unexpected list: " ++ show l

sslSupport :: IPv7 -> Bool
sslSupport IPv7 {address = addr, hypenet = hyp} =
  let addrABAs = map fromABAtoBAB . concatMap findABAs $ addr
      hypABAs = concatMap findABAs hyp
      common = addrABAs `intersect` hypABAs
   in not (null common)

solution2 :: String -> Int
solution2 = length . filter sslSupport . map parseLine . lines
