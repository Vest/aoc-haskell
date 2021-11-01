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

solution :: String -> String
solution _ = "output"
