module Day9 where

import Debug.Trace

extractInput :: String -> String
extractInput (l : ls) | l `elem` ['A' .. 'Z'] = l : extractInput ls
extractInput (l : ls)
  | l == '(' =
    let charsString = takeWhile (/= 'x') ls
        charsNum = read charsString :: Int
        repeatString = takeWhile (/= ')') . drop (length charsString + 1) $ ls
        repeatNum = read repeatString :: Int
        restString = drop (length (charsString ++ ['x'] ++ repeatString ++ [')'])) ls
        substring = take charsNum restString
        tailString = drop (length substring) restString
     in (concat . replicate repeatNum $ substring) ++ extractInput tailString
extractInput _ = ""

solution1 :: String -> Int
solution1 = length . extractInput

solution :: String -> String
solution = show . solution1
