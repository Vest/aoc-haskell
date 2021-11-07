module Day8 where

import Data.Bifunctor
import Data.List (isPrefixOf, transpose)
import Data.List.Split

data Token = Rect | Rotate | Row | Column | Value Int | Skip
  deriving (Eq, Show)

type AST = [Statement]

type Screen = [String]

data Statement = Rectangle Int Int | RotateRow Int Int | RotateColumn Int Int | NOP
  deriving (Eq, Show)

parseToTokens :: [Token] -> String -> [Token]
parseToTokens tokens "" = tokens
parseToTokens tokens str =
  let (token, skipToken) = getNextToken str
   in parseToTokens (tokens ++ [token]) (drop skipToken str)

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

parseStatement :: [Token] -> Statement
parseStatement [Rect, Value a, Skip, Value b] = Rectangle a b
parseStatement [Rotate, Column, Value x, Skip, Value y] = RotateColumn x y
parseStatement [Rotate, Row, Value y, Skip, Value x] = RotateRow y x
parseStatement _ = NOP

buildAST :: String -> AST
buildAST = map (parseStatement . parseToTokens []) . lines

generateScreen :: Int -> Int -> Screen
generateScreen cols rows = replicate rows (replicate cols '.')

executeStatement :: Statement -> Screen -> Screen
executeStatement (Rectangle width height) screen =
  zipWith
    ( \index line ->
        let lineLength = length line
         in if index > height
              then line
              else replicate (min width lineLength) '#' ++ drop width line
    )
    [1 ..]
    screen
executeStatement (RotateRow row shift) screen =
  zipWith
    ( \index line ->
        let lineLength = length line
         in if index == row
              then
                let longLine = cycle line
                 in take lineLength (drop (lineLength - shift) longLine)
              else line
    )
    [0 ..]
    screen
executeStatement (RotateColumn col shift) screen =
  transpose
    . executeStatement (RotateRow col shift)
    . transpose
    $ screen
executeStatement NOP screen = screen

solution1 :: Int -> Int -> String -> Int
solution1 width height input =
  let ast = buildAST input
      startScreen = generateScreen width height
   in length
        . filter (== '#')
        $ concat $ foldl (flip executeStatement) startScreen ast

solution :: String -> String
solution input = show (solution1 50 6 input) ++ ", Message is:\n" ++ solution2 50 6 input

solution2 :: Int -> Int -> String -> String
solution2 width height input =
  let ast = buildAST input
      startScreen = generateScreen width height
   in unlines $
        map (unwords . chunksOf 5) $
          foldl (flip executeStatement) startScreen ast
