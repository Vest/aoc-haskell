module Days.Day7 where

import Day7
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day7" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
      "day7" ~: "parseLine(abba[mnop]qrst)" ~: Day7.IPv7 {address = ["abba", "qrst"], hypenet = ["mnop"]} ~=? Day7.parseLine "abba[mnop]qrst",
      "day7" ~: "isABBA(hm)" ~: False ~=? Day7.isABBA "hm",
      "day7" ~: "isABBA(abba)" ~: True ~=? Day7.isABBA "abba",
      "day7" ~: "isABBA(qrst)" ~: False ~=? Day7.isABBA "qrst",
      "day7" ~: "isABBA(ioxxoj)" ~: True ~=? Day7.isABBA "ioxxoj",
      "day7" ~: "isABBA(aaaa)" ~: False ~=? Day7.isABBA "aaaa"
    ]
