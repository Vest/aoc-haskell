module Days.Day5 where

import Day5
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day5" ~: "solution1(5 10 25)" ~: "test" ~=? Day5.solution1 "5 10 25"
    ]
