{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day4 where

import Data.List
import qualified Data.Map as M
import Data.Maybe
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
      reversedName = drop (7 + 3 + 1) reversedLine
   in RawRoom
        { name = reverse reversedName,
          sectorID = reverse reversedSectorID,
          checksum = reverse reversedChecksum
        }

fromRawToRoom :: RawRoom -> Room
fromRawToRoom RawRoom {name = rawName, sectorID = rawSectorId, checksum = rawChecksum} =
  let name = M.fromListWith (+) [(c, 1) | c <- filter (/= '-') rawName]
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
solution input = (show . solution1 $ input) ++ ", " ++ (show . fst $ solution2 "north" input)

decryptCaesar :: Int -> String -> String
decryptCaesar step =
  let alphabet = ['a' .. 'z']
      lenAlphabet = length alphabet
   in map
        ( \a -> case elemIndex a alphabet of
            Just index -> alphabet !! ((index + step) `rem` lenAlphabet)
            Nothing -> ' '
        )

solution2 :: String -> String -> (Int, String)
solution2 textToFind =
  fromMaybe (-1, "Nothing")
    . find (\(_, decryptedName) -> textToFind `isInfixOf` decryptedName)
    . map (\(Room {name = _, sectorID, checksum = _}, name) -> (sectorID, decryptCaesar sectorID name))
    . filter (\(Room {name, checksum}, _) -> countChecksum name == checksum)
    . map ((\raw -> (fromRawToRoom raw, name (raw :: RawRoom))) . parseToRawRoom)
    . lines
