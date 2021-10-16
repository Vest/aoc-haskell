import Day1
import Test.HUnit

foo :: Int -> (Int, Int)
foo x = (1, x)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1, 3) (foo 3))

test2 :: Test
test2 = TestCase (assertEqual "parseStep(R1)" (Just (Day1.Right 1)) (Day1.parseStep "R1"))

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

tests' :: Test
tests' =
  test
    [ "day1" ~: "parseStep(R1)" ~: Just (Day1.Right 1) ~=? Day1.parseStep "R1",
      "day1" ~: "parseStep(R2)" ~: Just (Day1.Right 2) ~=? Day1.parseStep "R2",
      "day1" ~: "parseStep(L1)" ~: Just (Day1.Left 1) ~=? Day1.parseStep "L1",
      "day1" ~: "parseStep(L2)" ~: Just (Day1.Left 2) ~=? Day1.parseStep "L2",
      "day1" ~: "parseInput('R1, L2')" ~: [Day1.Right 1, Day1.Left 2] ~=? Day1.parseInput "R1, L2",
      "day1" ~: "rotate Rx" ~: [Day1.East, Day1.West, Day1.North, Day1.South] ~=? map (Day1.rotate (Day1.Right 3)) [Day1.North, Day1.South, Day1.West, Day1.East],
      "day1" ~: "rotate Lx" ~: [Day1.West, Day1.East, Day1.South, Day1.North] ~=? map (Day1.rotate (Day1.Left 3)) [Day1.North, Day1.South, Day1.West, Day1.East],
      "day1" ~: "moveSteps a circle" ~: (West, Position 0 0) ~=? Day1.moveSteps 5 (West, snd (Day1.moveSteps 5 (South, snd (Day1.moveSteps 5 (East, snd (Day1.moveSteps 5 (North, Position 0 0))))))),
      "day1" ~: "movement(R2, L3)" ~: (North, Position 3 2) ~=? Day1.movement (North, Position 0 0) [Day1.Right 2, Day1.Left 3],
      "day1" ~: "solution1('R5, L5, R5, R3')" ~: 12 ~=? Day1.solution1 "R5, L5, R5, R3"
    ]

main :: IO Counts
main = do
  _ <- runTestTT tests
  runTestTT tests'
