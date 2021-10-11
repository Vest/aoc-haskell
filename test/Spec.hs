import Lib
import Test.HUnit

foo :: Int -> (Int, Int)
foo x = (1, x)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1, 3) (foo 3))

test2 :: Test
test2 = TestCase (assertEqual "parseStep(R1)" (Just (Lib.Right 1)) (Lib.parseStep "R1"))

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

tests' :: Test
tests' =
  test
    [ "day1" ~: "parseStep(R1)" ~: Just (Lib.Right 1) ~=? Lib.parseStep "R1",
      "day1" ~: "parseStep(R2)" ~: Just (Lib.Right 2) ~=? Lib.parseStep "R2",
      "day1" ~: "parseStep(L1)" ~: Just (Lib.Left 1) ~=? Lib.parseStep "L1",
      "day1" ~: "parseStep(L2)" ~: Just (Lib.Left 2) ~=? Lib.parseStep "L2",
      "day1" ~: "parseInput('R1, L2')" ~: [Lib.Right 1, Lib.Left 2] ~=? Lib.parseInput "R1, L2",
      "day1" ~: "rotate Rx" ~: [Lib.East, Lib.West, Lib.North, Lib.South] ~=? map (Lib.rotate (Lib.Right 3)) [Lib.North, Lib.South, Lib.West, Lib.East],
      "day1" ~: "rotate Lx" ~: [Lib.West, Lib.East, Lib.South, Lib.North] ~=? map (Lib.rotate (Lib.Left 3)) [Lib.North, Lib.South, Lib.West, Lib.East],
      "day1" ~: "moveSteps a circle" ~: (West, Position 0 0) ~=? Lib.moveSteps 5 (West, snd (Lib.moveSteps 5 (South, snd (Lib.moveSteps 5 (East, snd (Lib.moveSteps 5 (North, Position 0 0))))))),
      "day1" ~: "movement(R2, L3)" ~: (North, Position 3 2) ~=? Lib.movement (North, Position 0 0) [Lib.Right 2, Lib.Left 3],
      "day1" ~: "solution1('R5, L5, R5, R3')" ~: 12 ~=? Lib.solution1 "R5, L5, R5, R3"
    ]

main :: IO Counts
main = do
  _ <- runTestTT tests
  runTestTT tests'
