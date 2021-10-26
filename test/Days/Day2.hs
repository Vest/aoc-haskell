module Days.Day2 where

import Day2
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day2" ~: "solution1(ULL\nRRDDD\nLURDL\nUUUUD)" ~: "1985" ~=? Day2.solution1 "ULL\nRRDDD\nLURDL\nUUUUD",
      "day2" ~: "solution2(ULL\nRRDDD\nLURDL\nUUUUD)" ~: "5DB3" ~=? Day2.solution2 "ULL\nRRDDD\nLURDL\nUUUUD",
      "day2" ~: "parseLine RRDDD" ~: [R, R, D, D, D] ~=? Day2.parseLine "RRDDD",
      "day2" ~: "detectPosition getKey 0 empty" ~: Position {x = 0, y = 0} ~=? Day2.detectPosition Day2.getKey Position {x = 0, y = 0} [],
      "day2" ~: "detectPosition getKey RRR" ~: Position {x = 1, y = 0} ~=? Day2.detectPosition Day2.getKey Position {x = 0, y = 0} [R, R, R],
      "day2" ~: "detectPosition getKey LLL" ~: Position {x = -1, y = 0} ~=? Day2.detectPosition Day2.getKey Position {x = 0, y = 0} [L, L, L],
      "day2" ~: "detectPosition getKey UUU" ~: Position {x = 0, y = 1} ~=? Day2.detectPosition Day2.getKey Position {x = 0, y = 0} [U, U, U],
      "day2" ~: "detectPosition getKey DDD" ~: Position {x = 0, y = -1} ~=? Day2.detectPosition Day2.getKey Position {x = 0, y = 0} [D, D, D],
      "day2" ~: "detectPosition getKey ULL" ~: Position {x = -1, y = 1} ~=? Day2.detectPosition Day2.getKey Position {x = 0, y = 0} [U, L, L],
      "day2" ~: "detectPosition getKey RRDDD" ~: Position {x = 1, y = -1} ~=? Day2.detectPosition Day2.getKey Position {x = -1, y = 1} [R, R, D, D, D],
      "day2" ~: "detectPosition getKey LURDL" ~: Position {x = 0, y = -1} ~=? Day2.detectPosition Day2.getKey Position {x = 1, y = -1} [L, U, R, D, L],
      "day2" ~: "detectPosition getKey UUUUD" ~: Position {x = 0, y = 0} ~=? Day2.detectPosition Day2.getKey Position {x = 0, y = -1} [U, U, U, U, D],
      "day2" ~: "solution1 empty" ~: "" ~=? Day2.solution1 "",
      "day2" ~: "solution1 empty twice" ~: "5" ~=? Day2.solution1 "\n",
      "day2" ~: "solution1 empty thrice :)" ~: "55" ~=? Day2.solution1 "\n\n",
      "day2" ~: "solution1 empty four times" ~: "555" ~=? Day2.solution1 "\n\n\n"
    ]
