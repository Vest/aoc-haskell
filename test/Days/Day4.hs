module Days.Day4 where

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
        ~=? Day4.parseToRawRoom "aaaaa-bbb-z-y-x-123[abxyz]"
    ]
