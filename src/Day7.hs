module Day7 where

import Data.List.Split (splitOneOf)

data IPv7 = IPv7
  { address,
    hypenet ::
      [String]
  }
  deriving (Show, Eq)

parseLine :: String -> IPv7
parseLine =
  foldl
    ( \IPv7 {address = add, hypenet = hyp} (num, chars) ->
        if even num
          then IPv7 {address = add ++ [chars], hypenet = hyp}
          else IPv7 {address = add, hypenet = hyp ++ [chars]}
    )
    IPv7 {address = [], hypenet = []}
    . zip [0 ..]
    . splitOneOf "[]"

isABBA :: String -> Bool
isABBA (a : b : c : d : abba) =
  (a == d && b == c && a /= b) || isABBA (b : c : d : abba)
isABBA _ = False

solution :: String -> String
solution _ = "output"
