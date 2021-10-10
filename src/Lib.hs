module Lib
  ( someFunc,
    parseInput,
    parseStep,
    Rotate (Right, Left),
    Direction (North, South, East, West),
    rotate,
  )
where

import Data.List.Split
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Rotate = Right Int | Left Int

data Position = Position
  { row :: Int,
    col :: Int
  }

data Direction = North | South | East | West

instance Eq Rotate where
  Lib.Right s1 == Lib.Right s2 = s1 == s2
  Lib.Left s1 == Lib.Left s2 = s1 == s2
  _ == _ = False

instance Show Rotate where
  show (Lib.Right steps) = "R" ++ show steps
  show (Lib.Left steps) = "L" ++ show steps

instance Eq Direction where
  North == North = True
  South == South = True
  East == East = True
  West == West = True
  _ == _ = False

instance Show Direction where
  show (North) = "N"
  show (South) = "S"
  show (East) = "E"
  show (West) = "W"

parseStep :: String -> Maybe Rotate
parseStep stepString = case rotationChar of
  'R' -> Just . Lib.Right $ steps
  'L' -> Just . Lib.Left $ steps
  _ -> Nothing
  where
    rotationChar = head stepString
    steps = read . tail $ stepString :: Int

parseInput :: String -> [Rotate]
parseInput = catMaybes . map parseStep . split (dropDelims . dropBlanks $ onSublist ", ")

rotate :: Rotate -> Direction -> Direction
rotate rot dir = case (rot, dir) of
  (Lib.Right _, North) -> East
  (Lib.Right _, South) -> West
  (Lib.Right _, East) -> South
  (Lib.Right _, West) -> North
  (Lib.Left _, North) -> West
  (Lib.Left _, South) -> East
  (Lib.Left _, East) -> North
  (Lib.Left _, West) -> South
