module Days.Day5 where

import Day5
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day5" ~: "solution1(abc)" ~: "18f47a30" ~=? Day5.solution1 "abc",
      "day5" ~: "getMD5(abc3231929)" ~: "00000155f8105dff7f56ee10fa9b9abd" ~=? Day5.getMD5 "abc" 3231929,
      "day5" ~: "getPassword(abc)" ~: "18f47a30" ~=? Day5.getPassword "abc"
    ]
