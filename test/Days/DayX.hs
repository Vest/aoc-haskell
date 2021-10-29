module Days.DayX where

import DayX
import Test.HUnit

tests' :: Test
tests' =
  test
    [
    "dayx" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing"
    {- "day5" ~: "solution1(5 10 25)" ~: "test" ~=? DayX.solution1 "5 10 25" -}
    ]
