{-# LANGUAGE DuplicateRecordFields #-}

module Days.Day4 where

import Data.Map (fromList)
import Day4
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day4" ~: "solution1(5 10 25)" ~: 0 ~=? Day4.solution1 "5 10 25",
      "day4" ~: "parseToRawRoom aaaaa-bbb-z-y-x-123[abxyz]"
        ~: RawRoom
          { name = "aaaaabbbzyx",
            sectorID = "123",
            checksum = "abxyz"
          }
        ~=? Day4.parseToRawRoom "aaaaa-bbb-z-y-x-123[abxyz]",
      "day4" ~: "fromRawToRoom aaaaa-bbb-z-y-x-123[abxyz]"
        ~: Room
          { name = fromList [('b', 3), ('a', 5), ('z', 1), ('x', 1), ('y', 1)],
            sectorID = 123,
            checksum = "abxyz"
          }
        ~=? (Day4.fromRawToRoom . Day4.parseToRawRoom $ "aaaaa-bbb-z-y-x-123[abxyz]"),
      "day4" ~: "countChecksum(aaaaa-bbb-z-y-x)" ~: "abxyz" ~=? (Day4.countChecksum . fromList $ [('b', 3), ('a', 5), ('z', 1), ('x', 1), ('y', 1)])
    ]
