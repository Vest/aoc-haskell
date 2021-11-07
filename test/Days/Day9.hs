module Days.Day9 where

import Day9
import Test.HUnit

tests' :: Test
tests' =
  test
    [
    "day9" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing"
    {- "day9" ~: "solution1(5 10 25)" ~: "test" ~=? DayX.solution1 "5 10 25" -}
    ]
