module Days.Day8 where

import Day8
import Test.HUnit

tests' :: Test
tests' =
  test
    [
    "day8" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing"
    {- "day8" ~: "solution1(5 10 25)" ~: "test" ~=? DayX.solution1 "5 10 25" -}
    ]
