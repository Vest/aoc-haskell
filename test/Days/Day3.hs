module Days.Day3 where

import Day3
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day3" ~: "solution1(5 10 25)" ~: 0 ~=? Day3.solution1 "5 10 25",
      "day3" ~: "parseLine('5 10 25')" ~: Just (5, 10, 25) ~=? Day3.parseLine "5 10 25",
      "day3" ~: "parseLine('5 10')" ~: Nothing ~=? Day3.parseLine "5 10",
      "day3" ~: "isValid('5 10 25')" ~: False ~=? Day3.isValid (5, 10, 25),
      "day3" ~: "isValid('5 16 20')" ~: True ~=? Day3.isValid (5, 16, 20),
      "day3" ~: "solution1('5 10 25\n5 16 20')" ~: 1 ~=? Day3.solution1 "5 10 25\n5 16 20",
      "day3" ~: "transposeList('(101,102,103)...')"
        ~: [ (101, 102, 103),
             (301, 302, 303),
             (501, 502, 503)
           ]
        ~=? Day3.transposeList
          [ (101, 301, 501),
            (102, 302, 502),
            (103, 303, 503)
          ]
    ]
