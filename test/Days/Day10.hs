module Days.Day10 where

import Day10
import Test.HUnit

tests' :: Test
tests' =
  test
    [
    "day10" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing"
    {- "day10" ~: "solution1(5 10 25)" ~: "test" ~=? Day10.solution1 "5 10 25" -}
    ]
