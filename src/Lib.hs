module Lib
  ( someFunc,
    parseStep,
    Rotate (Right, Left),
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Rotate = Right Integer | Left Integer

instance Eq Rotate where
  Lib.Right s1 == Lib.Right s2 = s1 == s2
  Lib.Left s1 == Lib.Left s2 = s1 == s2
  _ == _ = False

instance Show Rotate where
  show (Lib.Right steps) = "R" ++ show steps
  show (Lib.Left steps) = "L" ++ show steps

parseStep :: String -> Maybe Rotate
parseStep step = Just (Lib.Right 0)
  where
    s = head step
