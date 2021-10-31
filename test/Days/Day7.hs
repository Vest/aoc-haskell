module Days.Day7 where

import Day7
import Test.HUnit

tests' :: Test
tests' =
  test
    [
    "day7" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing"
    {- "day5" ~: "solution1(5 10 25)" ~: "test" ~=? DayX.solution1 "5 10 25" -}
    ]
