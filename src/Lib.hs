module Lib
  ( someFunc,
    parseInput,
    parseStep,
    Rotate (Right, Left),
  )
where

import Data.List.Split
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Rotate = Right Int | Left Int

instance Eq Rotate where
  Lib.Right s1 == Lib.Right s2 = s1 == s2
  Lib.Left s1 == Lib.Left s2 = s1 == s2
  _ == _ = False

instance Show Rotate where
  show (Lib.Right steps) = "R" ++ show steps
  show (Lib.Left steps) = "L" ++ show steps

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
