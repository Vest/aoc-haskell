module Days.Day2 where

import Day2
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day2" ~: "solution1(ULL\nRRDDD\nLURDL\nUUUUD)" ~: 1985 ~=? Day2.solution1 "ULL\nRRDDD\nLURDL\nUUUUD",
      "day2" ~: "step 5 U" ~: 2 ~=? Day2.step 5 U,
      "day2" ~: "parseLine RRDDD" ~: [R, R, D, D, D] ~=? Day2.parseLine "RRDDD",
      "day2" ~: "detectNumber 0 empty" ~: 5 ~=? Day2.detectNumber 0 [],
      "day2" ~: "detectNumber RRR" ~: 6 ~=? Day2.detectNumber 5 [R, R, R],
      "day2" ~: "detectNumber LLL" ~: 4 ~=? Day2.detectNumber 5 [L, L, L],
      "day2" ~: "detectNumber UUU" ~: 2 ~=? Day2.detectNumber 5 [U, U, U],
      "day2" ~: "detectNumber DDD" ~: 8 ~=? Day2.detectNumber 5 [D, D, D],
      "day2" ~: "detectNumber ULL" ~: 1 ~=? Day2.detectNumber 5 [U, L, L],
      "day2" ~: "detectNumber RRDDD" ~: 9 ~=? Day2.detectNumber 1 [R, R, D, D, D],
      "day2" ~: "detectNumber LURDL" ~: 8 ~=? Day2.detectNumber 9 [L, U, R, D, L],
      "day2" ~: "detectNumber UUUUD" ~: 5 ~=? Day2.detectNumber 8 [U, U, U, U, D],
      "day2" ~: "solution1 empty" ~: 5 ~=? Day2.solution1 "",
      "day2" ~: "solution1 empty twice" ~: 55 ~=? Day2.solution1 "\n",
      "day2" ~: "solution1 empty thrice :)" ~: 555 ~=? Day2.solution1 "\n\n",
      "day2" ~: "solution1 empty four times" ~: 5555 ~=? Day2.solution1 "\n\n\n"
    ]
