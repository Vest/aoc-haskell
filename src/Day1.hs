{-# LANGUAGE NamedFieldPuns #-}

module Day1 where

import Data.List.Split
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Rotate = Right Int | Left Int

data Position = Position
  { row,
    col ::
      Int
  }

data Direction = North | South | East | West

instance Eq Rotate where
  Day1.Right s1 == Day1.Right s2 = s1 == s2
  Day1.Left s1 == Day1.Left s2 = s1 == s2
  _ == _ = False

instance Show Rotate where
  show (Day1.Right steps) = "R" ++ show steps
  show (Day1.Left steps) = "L" ++ show steps

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

instance Eq Position where
  p1 == p2 = row p1 == row p2 && col p1 == col p2

instance Show Position where
  show (Day1.Position row col) = "(" ++ (show col) ++ ", " ++ (show row) ++ ")"

parseStep :: String -> Maybe Rotate
parseStep stepString = case rotationChar of
  'R' -> Just . Day1.Right $ steps
  'L' -> Just . Day1.Left $ steps
  _ -> Nothing
  where
    rotationChar = head stepString
    steps = read . tail $ stepString :: Int

parseInput :: String -> [Rotate]
parseInput = catMaybes . map parseStep . split (dropDelims . dropBlanks $ onSublist ", ")

rotate :: Rotate -> Direction -> Direction
rotate rot dir = case (rot, dir) of
  (Day1.Right _, North) -> East
  (Day1.Right _, South) -> West
  (Day1.Right _, East) -> South
  (Day1.Right _, West) -> North
  (Day1.Left _, North) -> West
  (Day1.Left _, South) -> East
  (Day1.Left _, East) -> North
  (Day1.Left _, West) -> South

moveSteps :: Int -> (Direction, Position) -> (Direction, Position)
moveSteps steps (North, pos) = (North, pos {row = row pos + steps})
moveSteps steps (East, pos) = (East, pos {col = col pos + steps})
moveSteps steps (West, pos) = (West, pos {col = col pos - steps})
moveSteps steps (South, pos) = (South, pos {row = row pos - steps})

movement :: (Direction, Position) -> [Rotate] -> (Direction, Position)
movement (dir, pos) [] = (dir, pos)
movement (dir, pos) (x : xpath) =
  let newDir = rotate x dir
      steps = case x of
        Day1.Right s -> s
        Day1.Left s -> s
      newPos = moveSteps steps (newDir, pos)
   in movement newPos xpath

findPath :: Position -> Int
findPath Position {row, col} = abs row + abs col

solution1 :: String -> Int
solution1 = findPath . snd . movement (North, Position 0 0) . parseInput
