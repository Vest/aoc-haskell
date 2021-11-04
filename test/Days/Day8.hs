module Days.Day8 where

import Day8
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day8" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
      "day8" ~: "safeStringToInt(a)" ~: (0, 0) ~=? Day8.safeStringToInt "a",
      "day8" ~: "safeStringToInt(5)" ~: (5, 1) ~=? Day8.safeStringToInt "5",
      "day8" ~: "safeStringToInt(10)" ~: (10, 2) ~=? Day8.safeStringToInt "10",
      "day8" ~: "safeStringToInt(123b)" ~: (123, 3) ~=? Day8.safeStringToInt "123b",
      "day8" ~: "safeStringToInt(123b1)" ~: (123, 3) ~=? Day8.safeStringToInt "123b1",
      "day8" ~: "getNextToken(rect 3x2)" ~: (Rect, 5) ~=? Day8.getNextToken "rect 3x2",
      "day8" ~: "getNextToken(3x2)" ~: (Value 3, 1) ~=? Day8.getNextToken "3x2",
      "day8" ~: "parseToTokens(rect 3x2)" ~: [Rect, Value 3, Skip, Value 2] ~=? Day8.parseToTokens [] "rect 3x2",
      "day8" ~: "parseToTokens(rotate column x=1 by 1)" ~: [Rotate, Column, Value 1, Skip, Value 1] ~=? Day8.parseToTokens [] "rotate column x=1 by 1",
      "day8" ~: "parseToTokens(rotate row y=0 by 4)" ~: [Rotate, Row, Value 0, Skip, Value 4] ~=? Day8.parseToTokens [] "rotate row y=0 by 4",
      "day8" ~: "parseStatement(rect 3x2)" ~: Rectangle 3 2 ~=? Day8.parseStatement (Day8.parseToTokens [] "rect 3x2"),
      "day8" ~: "parseStatement(rotate column x=1 by 1)" ~: RotateColumn 1 1 ~=? Day8.parseStatement (Day8.parseToTokens [] "rotate column x=1 by 1"),
      "day8" ~: "parseStatement(rotate row y=0 by 4)" ~: RotateRow 0 4 ~=? Day8.parseStatement (Day8.parseToTokens [] "rotate row y=0 by 4"),
      "day8" ~: "buildAST(sampleData)" ~: [Rectangle 3 2, RotateColumn 1 1, RotateRow 0 4, RotateColumn 1 1] ~=? Day8.buildAST "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"
    ]
