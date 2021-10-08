import Lib
import Test.HUnit

foo :: Int -> (Int, Int)
foo x = (1, x)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1, 2) (foo 3))

test2 :: Test
test2 = TestCase (assertEqual "parseStep(R1)" (Just (Lib.Right 1)) (Lib.parseStep "R1"))

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

tests' :: Test
tests' =
  test
    [ "test1" ~: "parseStep(R1)" ~: Just (Lib.Right 1) ~=? (Lib.parseStep "R1"),
      "test2" ~: "parseStep(R2)" ~: Just (Lib.Right 2) ~=? (Lib.parseStep "R2"),
      "test3" ~: "parseStep(L1)" ~: Just (Lib.Left 1) ~=? (Lib.parseStep "L1"),
      "test4" ~: "parseStep(L2)" ~: Just (Lib.Left 2) ~=? (Lib.parseStep "L2")
    ]

main :: IO Counts
main = do
  _ <- runTestTT tests
  runTestTT tests'
