module Days.Day7 where

import Day7
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day7" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
      "day7" ~: "parseLine(abba[mnop]qrst)" ~: Day7.IPv7 {address = ["abba", "qrst"], hypenet = ["mnop"]} ~=? Day7.parseLine "abba[mnop]qrst"
    ]
