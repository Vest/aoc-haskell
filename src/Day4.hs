{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day4 where

import qualified Data.Map as M
import Data.Sort

data RawRoom = RawRoom {name, sectorID, checksum :: String}
  deriving (Eq, Show)

data Room = Room
  { name :: M.Map Char Int,
    sectorID :: Int,
    checksum :: String
  }
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

fromRawToRoom :: RawRoom -> Room
fromRawToRoom RawRoom {name = rawName, sectorID = rawSectorId, checksum = rawChecksum} =
  let name = M.fromListWith (+) [(c, 1) | c <- rawName]
      sectorID = read rawSectorId :: Int
      checksum = rawChecksum
   in Room
        { name,
          sectorID,
          checksum
        }

countChecksum :: M.Map Char Int -> String
countChecksum =
  take 5
    . map fst
    . sortBy
      ( \(ca, fa) (cb, fb) ->
          let result = compare fb fa
           in case result of
                EQ -> compare ca cb
                _ -> result
      )
    . M.toList

solution1 :: String -> Int
solution1 =
  sum
    . map (\Room {sectorID} -> sectorID)
    . filter (\Room {name, checksum} -> countChecksum name == checksum)
    . map (fromRawToRoom . parseToRawRoom)
    . lines

solution :: String -> String
solution = show . solution1
