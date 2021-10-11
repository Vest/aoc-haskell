{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( someFunc,
    parseInput,
    parseStep,
    Rotate (Right, Left),
    Direction (North, South, East, West),
    rotate,
    Position (..),
    moveSteps,
    movement,
    solution1,
  )
where

import Data.List.Split
import Data.Maybe
import Debug.Trace

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

instance Eq Position where
  p1 == p2 = row p1 == row p2 && col p1 == col p2

instance Show Position where
  show (Lib.Position row col) = "(" ++ (show col) ++ ", " ++ (show row) ++ ")"

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
        Lib.Right s -> s
        Lib.Left s -> s
      newPos = moveSteps steps (newDir, pos)
   in movement newPos xpath

findPath :: Position -> Int
findPath (Position {row, col}) = abs row + abs col

solution1 :: String -> Int
solution1 input = findPath . snd . movement (North, Position 0 0) . parseInput $ input
