module Days.Day8 where

import Day8
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day8" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
      "day8" ~: "safeStringToInt(5)" ~: (5, 1) ~=? Day8.safeStringToInt "5",
      "day8" ~: "safeStringToInt(10)" ~: (10, 2) ~=? Day8.safeStringToInt "10",
      "day8" ~: "safeStringToInt(123b)" ~: (123, 3) ~=? Day8.safeStringToInt "123b",
      "day8" ~: "safeStringToInt(123b1)" ~: (123, 3) ~=? Day8.safeStringToInt "123b1"
    ]
