module Days.Day6 where

import Day6
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day6" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
      "day6" ~: "chooseMostFrequent(5 10 25)" ~: 'e' ~=? Day6.chooseMostFrequent "eeedadn",
      "day6" ~: "solution1(sampleData)" ~: "easter" ~=? Day6.solution1 "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar",
      "day6" ~: "solution2(sampleData)" ~: "advent" ~=? Day6.solution2 "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
    ]
