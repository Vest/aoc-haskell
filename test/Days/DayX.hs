module Days.DayX where

import DayX
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day5" ~: "solution1(5 10 25)" ~: "test" ~=? DayX.solution1 "5 10 25"
    ]
