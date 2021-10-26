{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day2 where

data Direction = U | D | L | R | Empty
  deriving (Eq, Show)

data Position = Position {x, y :: Int}
  deriving (Eq, Show)

{-1 2 3
  4 5 6
  7 8 9-}
getKey :: Position -> Maybe Char
getKey Position {x = -1, y = -1} = Just '7'
getKey Position {x = 0, y = -1} = Just '8'
getKey Position {x = 1, y = -1} = Just '9'
getKey Position {x = -1, y = 0} = Just '4'
getKey Position {x = 0, y = 0} = Just '5'
getKey Position {x = 1, y = 0} = Just '6'
getKey Position {x = -1, y = 1} = Just '1'
getKey Position {x = 0, y = 1} = Just '2'
getKey Position {x = 1, y = 1} = Just '3'
getKey Position {x = _, y = _} = Nothing

stepPosition :: Position -> Direction -> Position
stepPosition Position {x, y} dir =
  let newPos =
        Position
          { x =
              x + case dir of
                L -> -1
                R -> 1
                _ -> 0,
            y =
              y + case dir of
                U -> 1
                D -> -1
                _ -> 0
          }
      newKey = getKey newPos
   in case newKey of
        Nothing -> Position {x, y}
        Just _ -> newPos

parseLine :: String -> [Direction]
parseLine =
  filter (/= Empty)
    . map
      ( \case
          'U' -> U
          'D' -> D
          'L' -> L
          'R' -> R
          _ -> Empty
      )

detectPosition :: Position -> [Direction] -> Position
detectPosition = foldl stepPosition

solution1 :: String -> String
solution1 =
  snd
    . foldl
      ( \(startPos, answer) dirs ->
          let newPos = detectPosition startPos dirs :: Position
           in ( newPos,
                case getKey newPos of
                  Just key -> answer ++ [key]
                  Nothing -> answer
              )
      )
      (Day2.Position {x = 0, y = 0}, [])
    . map parseLine
    . lines

solution :: String -> String
solution = solution1
