module Days.Day9 where

import Day9
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "day9" ~: "solution1(nothing)" ~: "nothing" ~=? "nothing",
      "day9" ~: "extractInput(ADVENT)" ~: "ADVENT" ~=? Day9.extractInput "ADVENT",
      "day9" ~: "extractInput(A(1x5)BC)" ~: "ABBBBBC" ~=? Day9.extractInput "A(1x5)BC",
      "day9" ~: "extractInput((3x3)XYZ)" ~: "XYZXYZXYZ" ~=? Day9.extractInput "(3x3)XYZ",
      "day9" ~: "extractInput(A(2x2)BCD(2x2)EFG)" ~: "ABCBCDEFEFG" ~=? Day9.extractInput "A(2x2)BCD(2x2)EFG",
      "day9" ~: "extractInput((6x1)(1x3)A)" ~: "(1x3)A" ~=? Day9.extractInput "(6x1)(1x3)A",
      "day9" ~: "extractInput(X(8x2)(3x3)ABCY)" ~: "X(3x3)ABC(3x3)ABCY" ~=? Day9.extractInput "X(8x2)(3x3)ABCY",
      "day9" ~: "extractIndefinitely((3x3)XYZ)" ~: "XYZXYZXYZ" ~=? Day9.extractIndefinitely "(3x3)XYZ",
      "day9" ~: "extractIndefinitely(X(8x2)(3x3)ABCY)" ~: "XABCABCABCABCABCABCY" ~=? Day9.extractIndefinitely "X(8x2)(3x3)ABCY",
      "day9" ~: "extractIndefinitely((27x12)(20x12)(13x14)(7x10)(1x12)A)" ~: replicate (12 * 12 * 14 * 10 * 12) 'A' ~=? Day9.extractIndefinitely "(27x12)(20x12)(13x14)(7x10)(1x12)A",
      "day9" ~: "extractIndefinitely((25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN)" ~: 445 ~=? Day9.solution2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
    ]
