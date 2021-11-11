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
solution input = (show . solution1 $ input) ++ ", " ++ (show . solution2 $ input)

{- Don't need this function. It was left for our history -}
extractInputRecursively :: String -> String
extractInputRecursively (l : ls) | l `elem` ['A' .. 'Z'] = l : extractInputRecursively ls
extractInputRecursively (l : ls)
  | l == '(' =
    let charsString = takeWhile (/= 'x') ls
        charsNum = read charsString :: Int
        repeatString = takeWhile (/= ')') . drop (length charsString + 1) $ ls
        repeatNum = read repeatString :: Int
        restString = drop (length (charsString ++ ['x'] ++ repeatString ++ [')'])) ls
        substring = take charsNum restString
        extractedSubstring = extractInputRecursively substring
        tailString = drop (length substring) restString
     in (concat . replicate repeatNum $ extractedSubstring) ++ extractInputRecursively tailString
extractInputRecursively _ = ""

getLengthRecursively :: String -> Int
getLengthRecursively (l : ls) | l `elem` ['A' .. 'Z'] = 1 + getLengthRecursively ls
getLengthRecursively (l : ls)
  | l == '(' =
    let charsString = takeWhile (/= 'x') ls
        charsNum = read charsString :: Int
        repeatString = takeWhile (/= ')') . drop (length charsString + 1) $ ls
        repeatNum = read repeatString :: Int
        restString = drop (length (charsString ++ ['x'] ++ repeatString ++ [')'])) ls
        substring = take charsNum restString
        extractedLength = getLengthRecursively substring
        tailString = drop (length substring) restString
     in repeatNum * extractedLength + getLengthRecursively tailString
getLengthRecursively _ = 0

solution2 :: String -> Int
solution2 = getLengthRecursively
