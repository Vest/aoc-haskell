module Day8 where

import Data.List

data Token = Rect | Rotate | Row | Column | Value Int

getNextToken :: String -> (Token, Int)
getNextToken code
  | rectPrefix `isPrefixOf` code = (Rect, length rectPrefix)
  | rotatePrefix `isPrefixOf` code = (Rotate, length rotatePrefix)
  | rowPrefix `isPrefixOf` code = (Row, length rowPrefix)
  | columnPrefix `isPrefixOf` code = (Column, length columnPrefix)
  | otherwise = error $ "Invalid token at: " ++ code
  where
    rectPrefix = "rect "
    rotatePrefix = "rotate "
    rowPrefix = "row y="
    columnPrefix = "column x="

safeStringToInt :: String -> (Int, Int)
safeStringToInt str =
  let parsed =
        snd $
          foldl
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
            str
   in (read parsed :: Int, length parsed)

solution :: String -> String
solution _ = "output"
