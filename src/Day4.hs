module Day4 where

data RawRoom = RawRoom {name, sectorID, checksum :: String}
  deriving (Eq, Show)

parseToRawRoom :: String -> RawRoom
parseToRawRoom line =
  let reversedLine = reverse line
      reversedChecksum = take 5 $ drop 1 $ take 7 reversedLine
      reversedSectorID = take 3 $ drop 7 reversedLine
      reversedName = filter (/= '-') $ drop (7 + 3) reversedLine
   in RawRoom
        { name = reverse reversedName,
          sectorID = reverse reversedSectorID,
          checksum = reverse reversedChecksum
        }

solution1 :: String -> Int
solution1 _ = 1

solution :: String -> String
solution input = "test" ++ input
