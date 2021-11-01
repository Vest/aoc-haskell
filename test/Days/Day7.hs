module Days.Day7 where

import Day7
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day7" ~: "parseLine(abba[mnop]qrst)" ~: Day7.IPv7 {address = ["abba", "qrst"], hypenet = ["mnop"]} ~=? Day7.parseLine "abba[mnop]qrst",
      "day7" ~: "isABBA(hm)" ~: False ~=? Day7.isABBA "hm",
      "day7" ~: "isABBA(abba)" ~: True ~=? Day7.isABBA "abba",
      "day7" ~: "isABBA(qrst)" ~: False ~=? Day7.isABBA "qrst",
      "day7" ~: "isABBA(ioxxoj)" ~: True ~=? Day7.isABBA "ioxxoj",
      "day7" ~: "isABBA(aaaa)" ~: False ~=? Day7.isABBA "aaaa",
      "day7" ~: "isIPv7Valid(abba[mnop]qrst)" ~: True ~=? (Day7.isIPv7Valid . Day7.parseLine $ "abba[mnop]qrst"),
      "day7" ~: "solution1(sampleData)" ~: 2 ~=? Day7.solution1 "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn"
    ]
