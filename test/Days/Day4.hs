{-# LANGUAGE DuplicateRecordFields #-}

module Days.Day4 where

import Data.Map (fromList)
import Day4
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day4" ~: "solution1(sample)" ~: (123 + 987 + 404)
        ~=? Day4.solution1
          ( "aaaaa-bbb-z-y-x-123[abxyz]\n"
              ++ "a-b-c-d-e-f-g-h-987[abcde]\n"
              ++ "not-a-real-room-404[oarel]\n"
              ++ "totally-real-room-200[decoy]"
          ),
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
      "day4" ~: "countChecksum(aaaaa-bbb-z-y-x)" ~: "abxyz"
        ~=? ( Day4.countChecksum . fromList $
                [ ('b', 3),
                  ('a', 5),
                  ('z', 1),
                  ('x', 1),
                  ('y', 1)
                ]
            ),
      "day4" ~: "countChecksum(a-b-c-d-e-f-g-h)" ~: "abcde"
        ~=? ( Day4.countChecksum . fromList $
                [ ('a', 1),
                  ('c', 1),
                  ('d', 1),
                  ('b', 1),
                  ('e', 1),
                  ('f', 1),
                  ('h', 1),
                  ('g', 1)
                ]
            ),
            "day4" ~: "decryptCaesar(qzmt-zixmtkozy-ivhz, 343)" ~: "very encrypted name"
                    ~=? Day4.decryptCaesar 343 "qzmt-zixmtkozy-ivhz"
    ]
