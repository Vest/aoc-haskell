module Day8 where

import Data.Bifunctor
import Data.List

data Token = Rect | Rotate | Row | Column | Value Int | Skip
  deriving (Eq, Show)

parseToTokens :: String -> [Token] -> [Token]
parseToTokens "" tokens = tokens
parseToTokens str tokens =
  let (token, skipToken) = getNextToken str
   in parseToTokens (drop skipToken str) (tokens ++ [token])

getNextToken :: String -> (Token, Int)
getNextToken code
  | rectPrefix `isPrefixOf` code = (Rect, length rectPrefix)
  | rotatePrefix `isPrefixOf` code = (Rotate, length rotatePrefix)
  | rowPrefix `isPrefixOf` code = (Row, length rowPrefix)
  | columnPrefix `isPrefixOf` code = (Column, length columnPrefix)
  | skipX `isPrefixOf` code = (Skip, length skipX)
  | skipBy `isPrefixOf` code = (Skip, length skipBy)
  | snd intPrefix > 0 = first Value intPrefix
  | otherwise = error $ "Invalid token at: " ++ code
  where
    rectPrefix = "rect "
    rotatePrefix = "rotate "
    rowPrefix = "row y="
    columnPrefix = "column x="
    skipX = "x"
    skipBy = " by "
    intPrefix = safeStringToInt code

safeStringToInt :: String -> (Int, Int)
safeStringToInt str =
  let parsed =
        snd
          . foldl
            ( \(toStop, answer) c ->
                if toStop
                  then (True, answer)
                  else
                    ( if c `elem` ['0' .. '9']
                        then (False, answer ++ [c])
                        else (True, answer)
                    )
            )
            (False, [])
          $ str
   in if null parsed
        then (0, 0)
        else (read parsed :: Int, length parsed)

solution :: String -> String
solution _ = "output"
