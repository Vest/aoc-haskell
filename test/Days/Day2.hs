module Days.Day2 where

import Day2
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day2" ~: "solution1(ULL\nRRDDD\nLURDL\nUUUUD)" ~: 1985 ~=? Day2.solution1 "ULL\nRRDDD\nLURDL\nUUUUD"
    ]
